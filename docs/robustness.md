# Decoder robustness, measured on real calls

The bench takes recordings of calls that worked, degrades them with the
channel simulator, and asks how much of the decode survives. The
reference for each call is what that recording decodes to untouched, so
this measures the modem against its own best effort on that line rather
than against an ideal that never existed.

    REPLAY="cabal run -v0 modec -- replay" python3 scripts/diag/bench.py snr

## Replaying a recording

Pointing the V.22 receiver at the top of a WAV does not work, and it
fails in a way that looks like success: the receiver acquires on ringback
and the answer tone, locks to nothing, and returns a constant that the
descrambler turns into a page of `U` or `w` characters. A live modem
never does that -- the handshake starts the data receiver at the right
instant, at the right rate, in the right channel. `modec replay`
therefore runs the whole modem over the recording, which reproduces the
live decode exactly, banner for banner.

    cabal run modec -- replay --mode v22bis,v22 recordings/CALL.wav

The timeline goes to stderr and the bytes to stdout, so a replay reads
like the call log it reproduces. `--impair snr=18` degrades the recording
on the way in, which is what the sweep below drives, and `--mint` writes
the recording out again as a test fixture: twelve of these calls are in
`test/fixtures/live/` and run on every `cabal test`.

The far end's audio is fixed, so our transmissions go nowhere. That is
sound for these recordings because a live modec already drew out the
responses in them. What it cannot do is ask how the far end would have
answered something different.

## What a real call survives

One recorded call -- 2600.network at V.22bis 2400, the fixture at
`test/fixtures/live/2600-v22bis-2400.wav` -- put back through the
simulator. It decodes to a 51-byte banner untouched, so anything longer
than 51 bytes is junk the receiver passed on and anything shorter is
text it lost.

| | connected | bytes | banner |
|---|---|---|---|
| untouched | V.22bis 2400 | 51 | yes |
| `--impair ulaw=1` | V.22bis 2400 | 51 | yes |
| `--channel voip` | V.22bis 2400 | 51 | yes |
| `--impair impulse=8` | V.22bis 2400 | 51 | yes |
| `--impair impulse=40` | **never connected** | 0 | no |
| `--impair harm2=0.15` | V.22bis 2400 | 51 | yes |
| `--impair softclip=4` | V.22bis 2400 | 51 | yes |
| `--impair wow=0.3` | V.22bis 2400 | 148 | yes |
| `--impair flutter=0.3` | V.22bis 2400 | 425 | **no** |
| `--impair biterr=0.001` | V.22bis 2400 | 59 | yes |
| `--impair sing=0.004 --impair singgain=0.5` | V.22bis 2400 | 89 | yes |
| `--channel tape` | V.22bis 2400 | 167 | **no** |
| `--channel noisy` | V.22bis 2400 | 199 | yes |

Three things worth reading off that.

**Re-encoding through µ-law costs nothing**, and neither does the whole
`voip` profile. That is the right answer and it is the check that the
profile is honest: a recording put back through a model of the path it
already came down should not get worse.

**Impulse noise has a cliff.** Eight impulses a second is free and forty
does not connect at all -- not a degraded connection, no connection. It
is the sharpest threshold of anything in the simulator, which is why
impulse noise counts rather than averages on a real line.

**Flutter is worse than wow at the same percentage.** Both are 0.3 % of
speed and both therefore swing the carrier by the same ±7 Hz; wow does
it at 1 Hz and the carrier loop follows it, flutter does it at 25 Hz and
the loop cannot. The call stays up and delivers 425 bytes with the
banner destroyed -- which is the failure `--max-evm` exists to prevent
and does not catch here.

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
