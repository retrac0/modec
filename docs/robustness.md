# Decoder robustness, measured on real calls

The bench takes recordings of calls that worked, degrades them with the
channel simulator, and asks how much of the decode survives. The
reference for each call is what that recording decodes to untouched, so
this measures the modem against its own best effort on that line rather
than against an ideal that never existed.

    cabal exec -- ghc -O1 -package modec -o /tmp/replay scripts/diag/replay.hs
    REPLAY=/tmp/replay python3 scripts/diag/bench.py snr

## Replaying a recording

Pointing the V.22 receiver at the top of a WAV does not work, and it
fails in a way that looks like success: the receiver acquires on ringback
and the answer tone, locks to nothing, and returns a constant that the
descrambler turns into a page of `U` or `w` characters. A live modem
never does that -- the handshake starts the data receiver at the right
instant, at the right rate, in the right channel. `scripts/diag/replay.hs`
therefore runs the whole modem over the recording, which reproduces the
live decode exactly, banner for banner.

The far end's audio is fixed, so our transmissions go nowhere. That is
sound for these recordings because a live modec already drew out the
responses in them. What it cannot do is ask how the far end would have
answered something different.

## What the noise sweep found

Similarity to the untouched decode, at full-band SNR:

| Call | Standard | 30 dB | 24 dB | 20 dB | 17 dB | 14 dB |
|---|---|---|---|---|---|---|
| Kludge BBS | V.22 1200 | 100% | 100% | 100% | 100% | 100% |
| C64 Pub | V.22 1200 | 100% | 100% | 100% | 54% | 0% |
| Sursum Corda | V.22 1200 | 100% | 100% | 100% | 100% | 99% |
| Basement BBS | V.22bis 2400 | 97% | 96% | 95% | 69% | 15% |
| Empire of the Dragon | Bell 103 300 | 100% | 100% | 100% | 100% | 100% |

Bell 103 at 300 bit/s is untouched by anything the sweep does, which is
what 200 Hz of tone spacing buys. V.22bis at 2400 gives out first, as
16 points in the same space as 4 must.

Carrier frequency offset is clean to 10 Hz and fails at 15 Hz by not
connecting at all, rather than by connecting and delivering nonsense.
Clock offset is clean to 0.5 % -- 5000 ppm, where real modems are inside
100 -- and likewise fails by not connecting.

## The fix this bench produced

Two calls in the sweep decoded a banner correctly and then kept going:
A-Net Online returned 44 characters of `Synchronet External POTS Support
v1.32` followed by 96 characters of noise, and NIST ACTS returned 274
characters of pure noise. Neither is a demodulation failure. A-Net's far
end hung up: the recording's level falls from 0.047 to nothing between
21 and 22 seconds, and the receiver went on framing its own decisions
all the way down. ACTS had simply not converged yet when the framer was
already armed.

The receiver knows the difference, and says so:

| | decision error |
|---|---|
| locked, clean line | 0.01 -- 0.03 |
| 20 dB SNR, 97% of the text still readable | 0.35 |
| 17 dB SNR, half the text readable | 0.69 |
| converging after CONNECT | 1.0 -- 2.2 |
| carrier collapsing | 53 -- 122 |

So bytes are no longer handed to the DTE above `--max-evm`, default 1.0.
A modem that passes on characters its own receiver knows are worthless is
worse than one that passes none, because nothing downstream can tell the
two apart. The threshold is an option rather than a constant because it
is a judgement about what to do at 14 dB, where a quarter of the text is
right and the rest is not.

Effect on the recordings, delivered bytes before and after:

| Call | before | after |
|---|---|---|
| NIST ACTS (all noise) | 274 | 32 |
| A-Net Online (44 good, then a dying carrier) | 140 | 60 |
| C64 Pub | 3253 | 3204 |
| Kludge BBS | 664 | 664 |

Nothing readable was lost: Kludge is untouched, and C64 Pub's banner and
menu survive intact. What went is the noise. Similarity at 20 dB rose
from 97% to 100% for C64 Pub and from 93% to 100% for Sursum Corda,
because the junk that used to pad the output is no longer there.

Below about 15 dB the modem now delivers nothing rather than delivering a
quarter of the characters correctly. That is deliberate, and `--max-evm`
is how to disagree with it.
