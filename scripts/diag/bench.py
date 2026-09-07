#!/usr/bin/env python3
"""Degrade real recordings and see how much of the decode survives.

The reference is what the recording decodes to untouched, so this
measures robustness against the modem's own best effort on that call
rather than against an ideal that never existed.
"""
import difflib, os, subprocess, sys

REPLAY = os.environ.get("REPLAY", "cabal run -v0 modec -- replay").split()

CALLS = [
    ("Kludge BBS",     "kludge-bbs-v22",          "v22bis,v22"),
    ("C64 Pub",        "c64-pub-v22",             "v22bis,v22"),
    ("A-Net Online",   "a-net-online-v22bis",     "v22bis,v22"),
    ("Basement BBS",   "basement-bbs-v22bis-v8",  "v22bis,v22"),
    ("Sursum Corda",   "sursum-corda-v22bis",     "v22bis,v22"),
    ("Empire (B103)",  "empire-of-the-dragon-bell103", "bell103,v21"),
]

def newest(tag):
    hits = sorted((f for f in os.listdir("recordings") if f.endswith(tag + ".wav")), reverse=True)
    return os.path.join("recordings", hits[0]) if hits else None

def run(wav, modes, args):
    try:
        p = subprocess.run(REPLAY + ["--mode", modes] + args + [wav], capture_output=True, timeout=600)
    except subprocess.TimeoutExpired:
        return b"", "timeout"
    err = p.stderr.decode(errors="replace")
    std = "-"
    for line in err.splitlines():
        if "CONNECT" in line:
            std = line.split("CONNECT")[1].split()[0]
    return p.stdout, std

def score(ref, got):
    if not ref:
        return 0.0
    return difflib.SequenceMatcher(None, ref[:4000], got[:4000], autojunk=False).ratio()

def main(sweep):
    print("%-15s %-8s %6s  %s" % ("call", "clean", "bytes", "  ".join("%7s" % s for _, s in sweep)))
    print("-" * (34 + 9 * len(sweep)))
    for name, tag, modes in CALLS:
        wav = newest(tag)
        if not wav:
            print("%-15s (no recording)" % name); continue
        ref, std = run(wav, modes, [])
        cells = []
        for arg, _ in sweep:
            got, s2 = run(wav, modes, arg)
            r = score(ref, got)
            cells.append("%6.0f%%%s" % (100 * r, "!" if s2 == "-" else " "))
        print("%-15s %-8s %6d  %s" % (name[:15], std, len(ref), " ".join(cells)))
    print()
    print("percentages are similarity to the same call decoded untouched;")
    print("! marks a run that never reached CONNECT at all")

if __name__ == "__main__":
    which = sys.argv[1] if len(sys.argv) > 1 else "snr"
    if which == "snr":
        sweep = [(["--impair", "snr=%d" % d], "%ddB" % d) for d in (30, 24, 20, 17, 14, 11)]
    elif which == "freq":
        sweep = [(["--impair", "freq=%g" % h], "%gHz" % h) for h in (1, 2, 4, 7, 10, 15)]
    elif which == "rate":
        sweep = [(["--impair", "rate=%g" % r], "%g%%" % (100 * r)) for r in (.0005, .001, .002, .005, .01, .02)]
    else:
        sweep = [(["--impair", "%s=%s" % (which, v)], v) for v in sys.argv[2:]]
    main(sweep)
