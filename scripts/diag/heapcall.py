#!/usr/bin/env python3
"""Does the modem's heap grow while a call is up?

Runs the live modem loop -- `modec modem`, the same code a call runs,
not `replay`, which keeps everything it computes -- over a recorded
call as fast as it will go, with the runtime's heap census on, and says
how much was live through the run and how long the longest collection
took.  A collection takes as long as there is heap to go through, and
in a loop with 20 ms a block a long one is a block that goes out late:
a heap that climbs for as long as a phase lasts is a call that gets
later the longer it stays in it.

    heapcall.py RECORDING.wav [modem options]
    heapcall.py --s0            every mode's answered call from the bench's last S0 run

The recording is what the modem heard, from the moment the call came
up: a call's own `recordings/STAMP-....wav`.  The options are the
modem's -- `--answer`, `--mode v32bis`, `--v32-rate 14400`, `--v8`,
`--no-echo-data` -- and have to be the ones the call was made with, or
the start-up will not follow the recording.

What it found the first time is in docs/bench-tests.md: bounded
histories cut with `take` and never read, each one holding the block
before it.
"""
import glob, os, re, subprocess, sys, tempfile, wave

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))

def binary():
    found = glob.glob(os.path.join(ROOT, "dist-newstyle/build/*/ghc-*/modec-*/x/modec/build/modec/modec"))
    if not found: sys.exit("build modec first: cabal build exe:modec")
    return max(found, key=os.path.getmtime)

def census(wav, args):
    with tempfile.TemporaryDirectory() as tmp:
        w = wave.open(wav); raw = w.readframes(w.getnframes())
        open(os.path.join(tmp, "in.raw"), "wb").write(raw)
        r = subprocess.run([binary(), "modem"] + args + ["--audio-in", "in.raw", "--audio-out", os.devnull, "--data-stdio",
                            "+RTS", "-hT", "-i0.05", "-s", "-RTS"],
                           cwd=tmp, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        err = r.stderr.decode("latin1")
        samples = []; kinds = []; cur = None
        for l in open(os.path.join(tmp, "modec.hp"), errors="replace"):
            if l.startswith("BEGIN_SAMPLE"): cur = {}
            elif l.startswith("END_SAMPLE"): samples.append(sum(cur.values()) / 1e6); kinds.append(cur); cur = None
            elif cur is not None and "\t" in l:
                k, v = l.rsplit("\t", 1); cur[k] = int(v)
    conn = re.search(r"CONNECT ([^,\n]*)", err)
    gen1 = re.search(r"Gen  1 .*?(\d+\.\d+)s\s+(\d+\.\d+)s\s*$", err, re.M)
    n = len(samples); q = max(1, n // 8)
    top = kinds[max(range(n), key=lambda i: samples[i])] if n else {}
    return (conn.group(1) if conn else "no connect", len(raw) / 16000,
            [max(samples[i:i + q]) for i in range(0, n, q)], max(samples or [0]),
            float(gen1.group(2)) * 1000 if gen1 else float("nan"),
            sorted(top.items(), key=lambda kv: -kv[1])[:5])

def show(name, res, detail):
    conn, secs, eighths, peak, longest, top = res
    print("%-12s %-22s %5.1f s  live MB through the run: %s  peak %.1f  longest collection %.1f ms" % (
        name, conn, secs, " ".join("%.1f" % e for e in eighths), peak, longest))
    if detail:
        for k, v in top: print("      %7.2f MB  %s" % (v / 1e6, k.split(":")[-1][:70]))

S0 = {'bell103': ['--mode', 'bell103'], 'v21': ['--mode', 'v21'], 'bell212a': ['--mode', 'bell212a'], 'v22': ['--mode', 'v22'],
      'v22bis': ['--mode', 'v22bis'], 'v32-4800': ['--mode', 'v32', '--v32-rate', '4800'],
      'v32-9600': ['--mode', 'v32', '--v32-rate', '9600'], 'v32b-7200': ['--mode', 'v32bis', '--v32-rate', '7200'],
      'v32b-12000': ['--mode', 'v32bis', '--v32-rate', '12000'], 'v32b-14400': ['--mode', 'v32bis', '--v32-rate', '14400']}

def main(argv):
    if not argv or argv[0] in ("-h", "--help"):
        sys.exit(__doc__)
    if argv[0] == "--s0":
        for tag, args in S0.items():
            path = os.path.join(ROOT, "recordings/bench/sweep-suite-S0-%s-a.log" % tag)
            if not os.path.exists(path): print("%-12s no S0 log" % tag); continue
            m = re.search(r"recording this call to (\S+\.wav)", open(path, errors="replace").read())
            if not m: print("%-12s no recording named in its log" % tag); continue
            show(tag, census(os.path.join(ROOT, m.group(1)), ["--answer"] + args), False)
    else:
        show(os.path.basename(argv[0])[:12], census(argv[0], argv[1:]), True)

if __name__ == "__main__":
    main(sys.argv[1:])
