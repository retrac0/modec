#!/usr/bin/env python3
"""Take a MODEC_IO_TRACE apart: capture bursts, and how the loop kept up.

    MODEC_IO_TRACE=io.txt modec modem ...
    scripts/diag/iotrace.py io.txt [FROM_S TO_S]

The trace is one line per block: when the read began, when it returned,
when the write that answered it returned, samples read, silence the
cushion keeper added.  A read that had to wait is a burst arriving.  For
each burst this prints its blocks, the capture clock just after it
(samples delivered less time elapsed, which a loss at capture takes down
for good), how long the loop took to work through it, and how long until
the next -- a loop whose work is longer than that gap is behind the
audio.  Without a range, five seconds a line.
"""
import sys, collections, math

def main(argv):
    if not argv:
        print(__doc__); return
    rows = [l.split() for l in open(argv[0])]
    rows = [(float(r[0]), float(r[1]), float(r[2]), int(r[3]), int(r[4])) for r in rows if len(r) == 5]
    fs = 8000.0
    t0 = rows[0][1]
    bursts = []; cur = None; rd = 0
    for began, ret, wrote, n, extra in rows:
        if ret - began >= 0.002 or cur is None:
            if cur: bursts.append(cur)
            cur = [ret, 0, rd, wrote]
        cur[1] += 1; cur[3] = wrote
        rd += n
        if extra: print(f'  {extra} samples of silence added at {ret - t0:.3f} s')
    bursts.append(cur)
    print(f'{len(rows)} blocks in {len(bursts)} bursts over {rows[-1][1] - t0:.1f} s; blocks a burst:',
          dict(sorted(collections.Counter(b[1] for b in bursts).items())))
    out = []
    for i, (arr, k, before, done) in enumerate(bursts):
        gap = bursts[i + 1][0] - arr if i + 1 < len(bursts) else float('nan')
        out.append((arr - t0, k, ((before + k * 160) / fs - (arr - t0)) * 1000, (done - arr) * 1000, gap * 1000))
    if len(argv) >= 3:
        lo, hi = float(argv[1]), float(argv[2])
        for t, k, lvl, work, gap in out:
            if lo <= t <= hi:
                print(f'  {t:7.3f} s  {k:3d} blocks  clock {lvl:8.1f} ms  worked {work:6.1f} ms  next burst in {gap:6.1f} ms')
        return
    by = collections.defaultdict(list)
    for o in out: by[int(o[0] // 5)].append(o)
    for s in sorted(by):
        v = by[s]
        slack = [x[4] - x[3] for x in v if not math.isnan(x[4])]
        print(f'  {s * 5:3d}-{s * 5 + 5:3d} s: {len(v):3d} bursts  clock {min(x[2] for x in v):7.1f}..{max(x[2] for x in v):7.1f} ms  '
              f'longest work {max(x[3] for x in v):6.1f} ms  least slack {min(slack) if slack else float("nan"):6.1f} ms')

if __name__ == '__main__':
    main(sys.argv[1:])
