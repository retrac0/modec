#!/usr/bin/env python3
"""Does the PipeWire graph a call runs on lose capture, and when?

Builds the softphone path's audio with no modem and no telephone in it
-- a pw-loopback, a pw-cat playing silence into it, a pw-cat recording
the other side -- and stamps every arrival.  The capture clock is the
samples delivered less the time gone: it holds level while nothing is
lost, slopes if the graph is on a sound card's crystal, and steps down
by what was lost each time a graph cycle goes missing.  pw-mon runs
alongside, its events stamped as they arrive, so a step can be laid
against what the graph was doing.

    pwstall.py SECONDS [rt] [group NAME] [force FRAMES] [latency T]
                       [meter] [poke] [load N] [-v]

    rt            make both pw-cats' working threads real-time, through
                  rtkit, as modec does
    group NAME    node.group on every node (pipewire.dummy is the dummy
                  driver's own, and keeps the graph off the sound card)
    force FRAMES  node.force-quantum on the pw-cats
    latency T     what the pw-cats ask for (default 100ms)
    meter         attach a low-latency capture to the sink for six
                  seconds from 14 s, as a mixer window's level meter does
    poke          play a second of silence to the default sink every 12 s
    load N        run N busy loops beside it
    -v            list every graph event, not only those near a fault

What it found on the bench is in docs/bench-tests.md: pw-cat is not
real-time by itself, the graph follows whichever driver the desktop has
running, and every change of quantum costs capture.
"""
import os, re, select, signal, subprocess, sys, tempfile, threading, time, wave

args = sys.argv[1:]
if not args or args[0] in ("-h", "--help"):
    sys.exit(__doc__)
secs = float(args[0])
def opt(name, default=None):
    return args[args.index(name) + 1] if name in args else default
rt, poke, meter, verbose = "rt" in args, "poke" in args, "meter" in args, "-v" in args
nload = int(opt("load", "0"))
group = opt("group")
force = opt("force")
latency = opt("latency", "100ms")

FS = 8000
grp = (" node.group = " + group) if group else ""
frc = (" node.force-quantum = " + force) if force else ""
props = lambda n: "{ node.name = %s application.name = modec state.restore-props = false%s%s }" % (n, grp, frc)
lb = subprocess.Popen(["pw-loopback", "-n", "stall-lb",
    "--capture-props", "{ media.class = Audio/Sink node.name = stall-sink state.restore-props = false%s }" % grp,
    "--playback-props", "{ media.class = Audio/Source node.name = stall-src state.restore-props = false%s }" % grp])
time.sleep(1.0)
common = ["--raw", "--rate", str(FS), "--channels", "1", "--format", "s16", "--latency", latency]
play = subprocess.Popen(["pw-cat", "--playback", "--target", "stall-sink", "-P", props("stall-tx")] + common + ["-"],
                        stdin=subprocess.PIPE)
# stdbuf execs pw-cat, so this is pw-cat's pid
rec = subprocess.Popen(["stdbuf", "-o0", "pw-cat", "--record", "--target", "stall-src", "-P", props("stall-rx")] + common + ["-"],
                       stdout=subprocess.PIPE, bufsize=0)
if rt:
    time.sleep(0.5)
    for p in (play, rec):
        subprocess.run(["busctl", "call", "--system", "org.freedesktop.RealtimeKit1", "/org/freedesktop/RealtimeKit1",
                        "org.freedesktop.RealtimeKit1", "MakeThreadRealtimeWithPID", "ttu", str(p.pid), str(p.pid), "20"],
                       stdout=subprocess.DEVNULL)
burners = [subprocess.Popen([sys.executable, "-c", "import time\nt=time.time()+%f\nwhile time.time()<t: pass" % (secs + 4)])
           for _ in range(nload)]

events = []
mon = subprocess.Popen(["pw-mon"], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True, bufsize=1)
def watch():
    cur = None
    for line in mon.stdout:
        t = time.monotonic()
        s = line.rstrip()
        if s.startswith(("added:", "removed:", "changed:")):
            cur = [t, s.rstrip(":"), "", ""]
            events.append(cur)
        elif cur is not None:
            m = re.match(r"\s*\*?\s*id: (\d+)", s)
            if m and not cur[2]: cur[2] = m.group(1)
            m = re.match(r'\s*\*?\s*state: "?(\w+)"?', s)
            if m: cur[3] += " state=" + m.group(1)
            m = re.match(r'\s*\*?\s*(node\.name|application\.name|media\.class) = "(.*)"', s)
            if m: cur[3] += " %s=%s" % (m.group(1).split(".")[0], m.group(2))
threading.Thread(target=watch, daemon=True).start()

def feed():
    z = bytes(320)
    try:
        while True:
            play.stdin.write(z); play.stdin.flush()
    except Exception:
        pass
threading.Thread(target=feed, daemon=True).start()

marks = []
extras = []       # whatever else was started, so that nothing is left in the graph
def poker():
    z = os.path.join(tempfile.gettempdir(), "pwstall-silence.wav")
    w = wave.open(z, "wb"); w.setnchannels(2); w.setsampwidth(2); w.setframerate(48000); w.writeframes(bytes(48000 * 4)); w.close()
    time.sleep(8)
    while True:
        marks.append(time.monotonic())
        subprocess.run(["pw-play", z], stderr=subprocess.DEVNULL)
        time.sleep(11)
def metering():
    time.sleep(14)
    marks.append(time.monotonic())
    m = subprocess.Popen(["pw-cat", "--record", "--target", "stall-sink", "-P", "{ stream.capture.sink = true node.latency = 128/48000 }",
                          "--raw", "--rate", "48000", "--channels", "1", "--format", "s16", "-"], stdout=subprocess.DEVNULL)
    extras.append(m)
    time.sleep(6)
    m.terminate(); marks.append(time.monotonic())
if poke: threading.Thread(target=poker, daemon=True).start()
if meter: threading.Thread(target=metering, daemon=True).start()

# which driver the streams ride on, six seconds in
driver = []
def whose():
    time.sleep(6)
    out = subprocess.run(["pw-top", "-b", "-n", "2"], capture_output=True, text=True).stdout.split("S   ID  QUANT")[-1]
    cur = "?"
    for l in out.splitlines()[1:]:
        f = l.split()
        if not f: continue
        if "+" not in f and "=" not in f: cur = "%s (quantum %s)" % (f[-1], f[2])
        elif f[-1].startswith("stall-"): driver.append("%s on %s" % (f[-1], cur))
threading.Thread(target=whose, daemon=True).start()

fd = rec.stdout.fileno()
arr = []          # (returned, bytes so far, waited)
n = 0
t0 = None
last = time.monotonic()
end = last + secs + 3
while time.monotonic() < end:
    r, _, _ = select.select([fd], [], [], 1.0)
    if not r: continue
    b = os.read(fd, 65536)
    if not b: break
    t = time.monotonic()
    if t0 is None: t0 = t
    n += len(b)
    arr.append((t, n, t - last))
    last = t
for p in [rec, play, mon, lb] + burners + extras:
    try: p.send_signal(signal.SIGTERM)
    except Exception: pass
try: play.stdin.close()
except Exception: pass
if len(arr) < 10:
    sys.exit("no capture arrived: is PipeWire running, and are pw-cat and pw-loopback installed?")

settle = 3.0
clock = lambda t, nb: (nb / 2 / FS - (t - t0)) * 1000
tops = {}
for t, nb, w in arr:
    if t - t0 >= settle:
        k = int(t - t0)
        tops[k] = max(tops.get(k, -1e9), clock(t, nb))
lost = tops[min(tops)] - tops[max(tops)]
print("%s%s%s: capture lost %.1f ms in %.0f s at a load of %.1f" % (
    "real-time" if rt else "as pw-cat comes", ", group " + group if group else "", ", quantum forced to " + force if force else "",
    lost, arr[-1][0] - t0 - settle, os.getloadavg()[0]))
print("drivers:", "; ".join(driver) or "not listed")

# every loss, at the resolution of an arrival: the top of the clock over a quarter second
q = {}
for t, nb, w in arr:
    if t - t0 >= settle:
        k = int((t - t0) * 4)
        q[k] = max(q.get(k, -1e9), clock(t, nb))
ks = sorted(q)
falls = ["%.1f s %.1f ms" % (b / 4, q[b] - q[a]) for a, b in zip(ks, ks[1:]) if q[b] < q[a] - 2.5]
print("falls in the capture clock: %d%s" % (len(falls), (": " + ", ".join(falls[:40])) if falls else ""))

# the size capture arrives in is the quantum of whichever driver runs the streams
sizes = {}
for i in range(1, len(arr)):
    sizes.setdefault(int(arr[i][0] - t0), []).append((arr[i][1] - arr[i - 1][1]) // 2)
runs = []
for k in sorted(sizes):
    m = sorted(sizes[k])[len(sizes[k]) // 2]
    if not runs or runs[-1][1] != m: runs.append([k, m])
print("arrival size:", ", ".join("from %d s %d samples" % (k, m) for k, m in runs))

long = [(t - t0, w * 1000) for t, nb, w in arr if w > 0.045 and t - t0 > settle]
print("reads that waited over 45 ms:", ", ".join("%.2f s (%.0f ms)" % x for x in long) or "none")
if marks: print("pokes at:", ", ".join("%.2f" % (p - t0) for p in marks))
near = lambda t: any(abs(t - (x + t0)) < 1.0 for x, _ in long) or any(abs(t - p) < 1.5 for p in marks)
shown = [e for e in events if (verbose and e[0] - t0 > settle) or near(e[0])]
print("graph events%s: %d of %d" % ("" if verbose else " within a second of a long read or a poke", len(shown), len(events)))
for t, kind, i, what in shown:
    print("  %8.2f %-8s id %-4s%s" % (t - t0, kind, i, what))
