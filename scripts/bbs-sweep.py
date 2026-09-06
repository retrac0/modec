#!/usr/bin/env python3
"""Dial a list of BBS numbers with modec and record each call.

One call at a time.  For every call baresip is started fresh and killed
again afterwards, so a call cannot outlive the driver's own timer no
matter how the modem side behaves: the hard cap on connected time is
enforced by killing the user agent, not by asking it nicely.

baresip is started first because modec connects to its control port at
startup; the PipeWire loopback nodes modec then creates are in place well
before any call needs them, since baresip's audio module only opens the
device when media comes up.
"""
import os, re, signal, struct, subprocess, sys, time, datetime, json
import math

ROOT = "/home/joel/modec"
REC = os.path.join(ROOT, "recordings")
LOGS = os.environ.get("BBS_LOGS", "/tmp/bbs-logs")
DOMAIN = "toronto.voip.ms"
CTRL = 4444
# The trunk answers our INVITE straight away and plays ringback as early
# media, so "call established" is not "the BBS picked up".  Occupancy of
# the BBS's own line starts at its answer tone, and that is what the 20 s
# budget is spent on; the tone is picked out of the recording as it is
# written.  Ringing costs the far end nothing, so it is not counted, but
# it is still bounded.
ANSWER_CAP = float(os.environ.get("BBS_HOLD", "20"))   # seconds on the line once answered
NUDGE = 4.0             # seconds between bare returns once connected
RING_CAP = 32.0         # give up if no answer tone by then
TOTAL_CAP = ANSWER_CAP + RING_CAP + 10        # backstop on the whole call
# Each configuration names the modec arguments to negotiate with.  V.8
# has codepoints only for ITU modes, so a Bell-only configuration cannot
# use it and falls back to the classic ladder on its own.
CONFIGS = {
    "2400":        ["--modes", "v22bis"],
    "300":         ["--modes", "v21,bell103"],
    "v8":          ["--v8"],
    "v8all":       ["--v8-offer-all"],
    "v22bis":      ["--modes", "v22bis"],
    "v22bis-v8":   ["--modes", "v22bis", "--v8"],
    "v22":         ["--modes", "v22"],
    "bell212a":    ["--modes", "bell212a"],
    "v21":         ["--modes", "v21"],
    "v21-v8":      ["--modes", "v21", "--v8"],
    # V.23 duplex: we send 75 bit/s and receive 1200, so a banner arrives
    # in a quarter of the time a V.21 one does and anything typed back
    # crawls.  Boards that offer viewdata answer this.
    "v23":         ["--modes", "v23"],
    "v23-v8":      ["--modes", "v23", "--v8"],
    "bell103":     ["--modes", "bell103"],
    "auto":        [],
    "auto-v8":     ["--v8"],
    # MNP error correction.  Class 2 frames over the ordinary start-stop
    # characters; class 4 adds synchronous framing, the shorter headers and
    # adaptive frame sizing.  Both fall through to an unprotected
    # connection if the far end does not answer a link request, so a board
    # with no error correction still gets its banner through.
    "mnp2":        ["--modes", "v22bis", "--mnp-class", "2"],
    "mnp4":        ["--modes", "v22bis", "--mnp"],
}

def log(msg):
    print("%s  %s" % (time.strftime("%H:%M:%S"), msg), flush=True)

def killall(names):
    for n in names:
        subprocess.run(["pkill", "-x", n], capture_output=True)

class AnswerWatch:
    """Spot the far end answering by the tone it sends.

    Reads the growing RX recording and looks for the V.25 answer tone
    (2100 Hz) or the Bell one (2225 Hz) standing well above everything
    else.  Ringback is around 400-500 Hz and fails the test, which is the
    whole point: a ringing line is not an occupied one.

    1650 Hz is here for the V.21-only boards: an answering modem that
    sends no ANS at all still has to idle at its channel 2 mark, and
    without this the sweep would score such a board as never having
    picked up.
    """
    FREQS = (2100.0, 2225.0, 1650.0)
    FS = 8000
    FRAME = 320                       # 40 ms
    NEEDED = 8                        # 0.32 s of tone

    def __init__(self, path):
        self.path = path
        self.pos = 0
        self.run = 0
        self.tail = b""

    def answered(self):
        try:
            with open(self.path, "rb") as f:
                f.seek(self.pos if self.pos else 44)
                d = f.read()
                self.pos = f.tell()
        except OSError:
            return False
        d = self.tail + d
        n = len(d) // 2
        use = (n // self.FRAME) * self.FRAME
        self.tail = d[use * 2:]
        if not use:
            return False
        xs = struct.unpack("<%dh" % use, d[:use * 2])
        for i in range(0, use, self.FRAME):
            fr = xs[i:i + self.FRAME]
            total = sum(v * v for v in fr) / self.FRAME
            if total < (0.02 * 32768) ** 2:       # too quiet to judge
                self.run = 0
                continue
            best = 0.0
            for f0 in self.FREQS:
                w = 2 * math.cos(2 * math.pi * f0 / self.FS)
                s1 = s2 = 0.0
                for v in fr:
                    s0 = v + w * s1 - s2
                    s2, s1 = s1, s0
                p = (s1 * s1 + s2 * s2 - w * s1 * s2) / (self.FRAME ** 2 / 4)
                best = max(best, p)
            self.run = self.run + 1 if best > 0.35 * total else 0
            if self.run >= self.NEEDED:
                return True
        return False


def send(proc, data):
    try:
        proc.stdin.write(data); proc.stdin.flush()
    except Exception:
        pass


def place_call(binpath, number, label, rate, outdir):
    """Returns a dict describing what happened."""
    killall(["baresip", "modec", "pw-loopback", "pw-cat"])
    time.sleep(1.0)
    ts = datetime.datetime.now().strftime("%Y%m%dT%H%M%S")
    safe = re.sub(r"[^A-Za-z0-9]+", "-", label).strip("-").lower()
    wav = os.path.join(outdir, "%s-%s-%s.wav" % (ts, safe, rate))
    res = {"bbs": label, "number": number, "rate": rate, "wav": os.path.basename(wav),
           "started": ts, "connected": False, "standard": None, "text": "",
           "outcome": "no answer", "call_seconds": 0.0, "ring_seconds": 0.0,
           "connect_seconds": None, "v8": None,
           "mnp": None, "mnp_seconds": None, "mnp_outcome": None}

    baresip = subprocess.Popen(["baresip"], stdout=subprocess.DEVNULL,
                               stderr=subprocess.DEVNULL, stdin=subprocess.DEVNULL)
    time.sleep(3.5)                            # let it register
    modec = subprocess.Popen(
        [binpath, "modem", "--sip", "127.0.0.1:%d" % CTRL, "--sip-domain", DOMAIN,
         "--audio-sip-loop", "modec"]
        + CONFIGS[rate]
        + ["--record-rx", wav, "--data-stdio"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        cwd=ROOT, bufsize=0)
    os.set_blocking(modec.stdout.fileno(), False)
    os.set_blocking(modec.stderr.fileno(), False)
    out, err = b"", b""

    def pump():
        nonlocal out, err
        for src, which in ((modec.stdout, "o"), (modec.stderr, "e")):
            try:
                d = src.read()
            except Exception:
                d = None
            if d:
                if which == "o": out += d
                else: err += d

    try:
        # wait for the loopback nodes, then bring up the user agent
        t0 = time.time()
        while time.time() - t0 < 10 and b"Hayes command mode" not in err:
            pump(); time.sleep(0.1)
        if b"Hayes command mode" not in err:
            res["outcome"] = "modec did not start"
            return res, err.decode(errors="replace")

        modec.stdin.write(b"ATDT%s\r" % number.encode())
        modec.stdin.flush()
        log("  dialled %s" % number)

        # ringing: wait for the far end's answer tone, not the trunk's
        # early media
        dialled = time.time(); watch = AnswerWatch(wav); up = None
        while time.time() - dialled < RING_CAP:
            pump()
            if b"NO CARRIER" in out or b"BUSY" in out:
                res["outcome"] = "busy or rejected"; break
            if watch.answered() or re.search(rb"CONNECT \d+", out):
                up = time.time(); break
            time.sleep(0.1)
        if up is None:
            res["ring_seconds"] = round(time.time() - dialled, 1)
            if res["outcome"] == "no answer":
                log("  no answer tone after %.0f s" % res["ring_seconds"])
            return res, err.decode(errors="replace")
        res["ring_seconds"] = round(up - dialled, 1)
        log("  far end answered after %.1f s, holding %.0f s" % (res["ring_seconds"], ANSWER_CAP))
        nudged = 0.0
        while time.time() - up < ANSWER_CAP and time.time() - dialled < TOTAL_CAP:
            pump()
            if not res["connected"]:
                m = re.search(rb"CONNECT (\d+)", out)
                if m:
                    res["connected"] = True
                    res["connect_seconds"] = round(time.time() - up, 1)
                    sm = re.search(rb"CONNECT ([A-Za-z0-9]+) ", err)
                    res["standard"] = sm.group(1).decode() if sm else None
                    log("  CONNECT %s after %.1f s" % (m.group(1).decode(), res["connect_seconds"]))
                    nudged = time.time()
                    send(modec, b"\r")
            elif time.time() - nudged >= NUDGE:
                # a BBS waits at a prompt; a bare return is enough to walk
                # a banner into a menu without answering anything for real
                nudged = time.time()
                send(modec, b"\r")
            if b"NO CARRIER" in out and res["connected"]:
                break
            time.sleep(0.1)
        res["call_seconds"] = round(time.time() - up, 1)
        pump()
        try:
            modec.stdin.write(b"+++"); modec.stdin.flush(); time.sleep(1.2)
            modec.stdin.write(b"ATH\r"); modec.stdin.flush(); time.sleep(0.8)
        except Exception:
            pass
        pump()
    finally:
        if baresip:
            baresip.send_signal(signal.SIGTERM)
            try: baresip.wait(3)
            except subprocess.TimeoutExpired: baresip.kill()
        modec.send_signal(signal.SIGTERM)
        try: modec.wait(4)
        except subprocess.TimeoutExpired: modec.kill()
        killall(["baresip", "modec", "pw-loopback", "pw-cat"])
        pump()

    # printable text the far end sent
    txt = re.sub(rb"\xff[\xfa-\xfe].", b"", out)
    txt = bytes(c for c in txt if 32 <= c < 127 or c in (10, 13))
    res["text"] = txt.decode(errors="replace").strip()
    if res["connected"]:
        res["outcome"] = "connected"
    elif res["outcome"] == "no answer":
        res["outcome"] = "answered, no carrier"
    if res["standard"] is None:
        sm = re.search(rb"CONNECT ([A-Za-z0-9]+) ", err)
        if sm:
            res["standard"] = sm.group(1).decode()
    vm = re.search(rb"V\.8 far end offers: (.+)", err)
    if vm:
        res["v8"] = vm.group(1).decode(errors="replace").strip()
    # what the error-correcting protocol did, if it was offered at all
    mm = re.search(rb"MNP class (\d+), (\d+) outstanding frames, N401 (\d+)", err)
    if mm:
        res["mnp"] = "class %s, k=%s, N401=%s" % tuple(x.decode() for x in mm.groups())
        res["mnp_outcome"] = "up"
    elif re.search(rb"no error correction: the far end did not answer", err):
        res["mnp_outcome"] = "far end did not answer a link request"
    dm = re.search(rb"MNP link down: (.+)", err)
    if dm:
        res["mnp_outcome"] = "down: " + dm.group(1).decode(errors="replace").strip()
    return res, err.decode(errors="replace")

if __name__ == "__main__":
    os.makedirs(LOGS, exist_ok=True)
    binpath = subprocess.run(["cabal", "list-bin", "exe:modec"], cwd=ROOT,
                             capture_output=True, text=True).stdout.strip()
    targets = json.load(open(sys.argv[1]))
    rates = sys.argv[2].split(",") if len(sys.argv) > 2 else ["2400", "300"]
    results = []
    outjson = sys.argv[3] if len(sys.argv) > 3 else "/dev/null"
    # Rotating pairs each BBS with a different configuration instead of
    # running the whole cross product: it covers every configuration
    # across the population without calling one BBS a dozen times over.
    rotate = os.environ.get("BBS_ROTATE") == "1"
    offset = int(os.environ.get("BBS_OFFSET", "0"))
    plan = ([(t, rates[(i + offset) % len(rates)]) for i, t in enumerate(targets)]
            if rotate else [(t, r) for t in targets for r in rates])
    for t, rate in plan:
        if True:
            log("%s  %s  @%s" % (t["name"], t["number"], rate))
            r, errlog = place_call(binpath, t["number"], t["name"], rate, REC)
            log("  -> %s%s" % (r["outcome"], (" " + r["standard"]) if r["standard"] else ""))
            if r["v8"]:
                log("  V.8: %s" % r["v8"])
            if r["mnp"] or r["mnp_outcome"]:
                log("  MNP: %s" % (r["mnp"] or r["mnp_outcome"]))
            if r["text"]:
                log("  text: %r" % r["text"][:200])
            with open(os.path.join(LOGS, os.path.basename(r["wav"]) + ".log"), "w") as f:
                f.write(errlog)
            results.append(r)
            json.dump(results, open(outjson, "w"), indent=1)
    log("done, %d calls" % len(results))
