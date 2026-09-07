#!/usr/bin/env python3
"""Dial a list of BBS numbers with modec and record each call.

One call at a time, through one baresip that is started once and kept
registered for the whole sweep.

It used to be started fresh for every call, which made the hard cap on
call length easy -- killing the user agent ends the call whatever the
modem side is doing -- but it also meant one SIP REGISTER per call, and
a trunk that answers the first dozen of those will start dropping them.
On a sweep of twenty numbers, six in a row came back "no answer" with
nothing dialled at all: REGISTER had timed out and the calls were never
placed.  So registration happens once, and the cap is enforced by
killing modec and then telling baresip to hang up over its own control
port, which clears the line without touching the registration.

baresip is started before modec because modec connects to its control
port at startup; the PipeWire loopback nodes modec then creates are in
place well before any call needs them, since baresip's audio module only
opens the device when media comes up.
"""
import os, re, signal, socket, struct, subprocess, sys, time, datetime, json
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
# baresip has to finish registering before a number can be dialled, or
# it answers the dial with "could not find UA" and the call is scored as
# a no-answer that was never placed.  Waited for rather than slept
# through: a fixed pause is a guess, and the guess was wrong on a cold
# start.
REG_WAIT = float(os.environ.get("BBS_REG_WAIT", "40"))  # seconds to wait for registration
GAP = float(os.environ.get("BBS_GAP", "0"))             # seconds between calls
BARESIP_LOG = os.environ.get("BBS_BARESIP_LOG", "/tmp/bbs-baresip.log")
NUDGE = 4.0             # seconds between bare returns once connected
RING_CAP = 32.0         # give up if no answer tone by then
TOTAL_CAP = ANSWER_CAP + RING_CAP + 10        # backstop on the whole call
# Each configuration names the modec arguments to negotiate with.  V.8
# has codepoints only for ITU modes, so a Bell-only configuration cannot
# use it and falls back to the classic ladder on its own.
CONFIGS = {
    "2400":        ["--mode", "v22bis"],
    "300":         ["--mode", "v21,bell103"],
    "v8":          ["--v8"],
    "v8all":       ["--v8-offer-all"],
    "v22bis":      ["--mode", "v22bis"],
    "v22bis-v8":   ["--mode", "v22bis", "--v8"],
    "v22":         ["--mode", "v22"],
    "bell212a":    ["--mode", "bell212a"],
    "v21":         ["--mode", "v21"],
    "v21-v8":      ["--mode", "v21", "--v8"],
    # V.23 duplex: we send 75 bit/s and receive 1200, so a banner arrives
    # in a quarter of the time a V.21 one does and anything typed back
    # crawls.  Boards that offer viewdata answer this.
    "v23":         ["--mode", "v23"],
    "v23-v8":      ["--mode", "v23", "--v8"],
    "bell103":     ["--mode", "bell103"],
    "auto":        [],
    "auto-v8":     ["--v8"],
    # MNP error correction.  Class 2 frames over the ordinary start-stop
    # characters; class 4 adds synchronous framing, the shorter headers and
    # adaptive frame sizing.  Both fall through to an unprotected
    # connection if the far end does not answer a link request, so a board
    # with no error correction still gets its banner through.
    "mnp2":        ["--mode", "v22bis", "--mnp-class", "2"],
    "mnp4":        ["--mode", "v22bis", "--mnp"],
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


def ctrl_send(cmd, params=""):
    """One command down baresip's ctrl_tcp socket, netstring-framed JSON.

    Used only between calls, when modec has exited and is no longer
    holding the port: it is how a call that outlived its budget is
    cleared without restarting the user agent and losing the
    registration with it.
    """
    msg = json.dumps({"command": cmd, "params": params, "token": "sweep"})
    payload = ("%d:%s," % (len(msg), msg)).encode()
    try:
        s = socket.create_connection(("127.0.0.1", CTRL), timeout=3)
        s.sendall(payload)
        time.sleep(0.3)
        s.close()
        return True
    except OSError:
        return False


class Agent:
    """One baresip, started once and kept registered for the sweep."""

    def __init__(self, path=BARESIP_LOG):
        self.path = path
        self.proc = None
        self.mark = 0

    def _lines(self):
        try:
            with open(self.path, "rb") as f:
                f.seek(self.mark)
                return f.read().decode(errors="replace").splitlines()
        except OSError:
            return []

    def registered(self):
        # baresip prints the registrar's answer; 200 OK is a registration
        # that took.  Failures are left to the timeout rather than
        # matched, since what baresip prints for them varies.
        return any("200 OK" in l for l in self._lines())

    def alive(self):
        return self.proc is not None and self.proc.poll() is None

    def start(self):
        self.stop()
        killall(["baresip"])
        time.sleep(0.5)
        self.mark = os.path.getsize(self.path) if os.path.exists(self.path) else 0
        f = open(self.path, "ab")
        self.proc = subprocess.Popen(["baresip"], stdout=f, stderr=subprocess.STDOUT,
                                     stdin=subprocess.DEVNULL)
        t0 = time.time()
        while time.time() - t0 < REG_WAIT:
            if self.registered():
                log("  baresip registered after %.1f s" % (time.time() - t0))
                return True
            if not self.alive():
                break
            time.sleep(0.5)
        log("  baresip did not register within %.0f s" % REG_WAIT)
        for l in self._lines()[-5:]:
            log("    %s" % l)
        return False

    def ensure(self):
        """Registered and running, restarting with backoff if not."""
        if self.alive() and self.registered():
            return True
        for wait in (0, 15, 45):
            if wait:
                log("  waiting %.0f s before trying to register again" % wait)
                time.sleep(wait)
            if self.start():
                return True
        return False

    def stop(self):
        if self.proc is not None:
            self.proc.send_signal(signal.SIGTERM)
            try:
                self.proc.wait(3)
            except subprocess.TimeoutExpired:
                self.proc.kill()
            self.proc = None


def send(proc, data):
    try:
        proc.stdin.write(data); proc.stdin.flush()
    except Exception:
        pass


def place_call(agent, binpath, number, label, rate, outdir):
    """Returns a dict describing what happened."""
    killall(["modec", "pw-loopback", "pw-cat"])
    time.sleep(1.0)
    ts = datetime.datetime.now().strftime("%Y%m%dT%H%M%S")
    safe = re.sub(r"[^A-Za-z0-9]+", "-", label).strip("-").lower()
    wav = os.path.join(outdir, "%s-%s-%s.wav" % (ts, safe, rate))
    res = {"bbs": label, "number": number, "rate": rate, "wav": os.path.basename(wav),
           "started": ts, "connected": False, "standard": None, "text": "",
           "outcome": "no answer", "call_seconds": 0.0, "ring_seconds": 0.0,
           "connect_seconds": None, "v8": None,
           "mnp": None, "mnp_seconds": None, "mnp_outcome": None}

    if not agent.ensure():
        res["outcome"] = "not dialled: SIP registration failed"
        return res, "baresip would not register\n"
    modec = subprocess.Popen(
        [binpath, "modem", "--sip", "127.0.0.1:%d" % CTRL, "--sip-domain", DOMAIN,
         "--audio-sip-loop", "modec"]
        + CONFIGS[rate]
        + ["--record-rx", wav, "--no-record", "--data-stdio"],
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

        # A registration that failed is not a BBS that did not answer.
        # Telling the two apart is the whole value of the sweep: a
        # number scored "no answer" when nothing was ever dialled is
        # worse than no result, because it looks like a result.
        pump()
        if b"REGISTER_FAIL" in err:
            res["outcome"] = "not dialled: SIP registration failed"
            return res, err.decode(errors="replace")

        modec.stdin.write(b"ATDT%s\r" % number.encode())
        modec.stdin.flush()
        log("  dialled %s" % number)

        # ringing: wait for the far end's answer tone, not the trunk's
        # early media
        dialled = time.time(); watch = AnswerWatch(wav); up = None
        while time.time() - dialled < RING_CAP:
            pump()
            if b"BUSY" in out:
                res["outcome"] = "busy, congestion or special information tone"; break
            if b"NO CARRIER" in out:
                res["outcome"] = "busy or rejected"; break
            if b"REGISTER_FAIL" in err or b"could not find UA" in err:
                res["outcome"] = "not dialled: SIP registration failed"; break
            if watch.answered() or re.search(rb"CONNECT \d+", out):
                up = time.time(); break
            time.sleep(0.1)
        if up is None:
            res["ring_seconds"] = round(time.time() - dialled, 1)
            if res["outcome"].startswith("not dialled"):
                log("  %s" % res["outcome"])
            elif res["outcome"] == "no answer":
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
        modec.send_signal(signal.SIGTERM)
        try: modec.wait(4)
        except subprocess.TimeoutExpired: modec.kill()
        killall(["modec", "pw-loopback", "pw-cat"])
        pump()
        # modec is gone and cannot have sent a BYE, so make sure the
        # line is down before the next number is dialled.  The port is
        # free now that modec has exited.
        ctrl_send("hangup")

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
    agent = Agent()
    try:
      for t, rate in plan:
        if True:
            log("%s  %s  @%s" % (t["name"], t["number"], rate))
            r, errlog = place_call(agent, binpath, t["number"], t["name"], rate, REC)
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
            if GAP:
                time.sleep(GAP)
    finally:
      agent.stop()
      killall(["baresip", "modec", "pw-loopback", "pw-cat"])
    log("done, %d calls" % len(results))
