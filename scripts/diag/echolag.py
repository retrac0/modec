#!/usr/bin/env python3
"""Where our own echo is in a recorded call, two seconds at a time.

    scripts/diag/echolag.py recordings/STAMP-1001 [FROM_S [TO_S [STEP_S]]]

Correlates the call's transmit recording (STEM-tx.wav) against what it
received (STEM.wav) at every lag to 950 ms, window by window, and prints
the best lag, its score, the mean score and the best score anywhere else.
A reflection is one lag that keeps coming back; its delay stepping in
the middle of a call is silence written into our transmit (the cushion
keeper) or an underrun, by exactly the step; and no lag standing out is
no echo -- an ATA's own canceller doing its job, on the bench.
"""
import sys, wave
import numpy as np

def read(path):
    w = wave.open(path)
    x = np.frombuffer(w.readframes(w.getnframes()), dtype='<i2').astype(float) / 32768
    return w.getframerate(), x

def main(argv):
    if not argv:
        print(__doc__); return
    stem = argv[0]
    fs, rx = read(stem + '.wav'); _, tx = read(stem + '-tx.wav')
    # the transmit file opens a block before the receive one: line them up by their ends
    if 0 < len(tx) - len(rx) <= 0.04 * fs:
        tx = tx[len(tx) - len(rx):]
    t0 = float(argv[1]) if len(argv) > 1 else 0.0
    t1 = float(argv[2]) if len(argv) > 2 else len(rx) / fs
    step = float(argv[3]) if len(argv) > 3 else 1.0
    W = int(2.0 * fs); reach = int(0.95 * fs)
    print(f'{stem}: {len(rx) / fs:.2f} s')
    t = t0
    while t + 2.0 <= t1:
        a = int(t * fs)
        r = rx[a:a + W]; r = r - r.mean()
        seg = tx[max(0, a - reach):a + W]
        seg = np.concatenate([np.zeros(reach - (a - max(0, a - reach))), seg])
        c = np.correlate(seg, r, mode='valid')
        e = np.sqrt(np.convolve(seg * seg, np.ones(W), mode='valid'))
        s = np.where(e > 1e-6, np.abs(c) / (e * np.linalg.norm(r) + 1e-12), 0)
        lags = reach - np.arange(len(s))
        ok = lags > 160
        k = np.argmax(np.where(ok, s, 0))
        rival = np.max(np.where(ok & (np.abs(lags - lags[k]) > 160), s, 0))
        print(f'{t:6.1f} s  lag {lags[k]:5d} samples {lags[k] * 1000 / fs:7.1f} ms  score {s[k]:.4f}  mean {s[ok].mean():.4f}  rival {rival:.4f}')
        t += step

if __name__ == '__main__':
    main(sys.argv[1:])
