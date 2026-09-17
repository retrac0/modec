# Bench test plan

What to run against the hardware bench -- the CX93001 behind the HT802V2,
modec behind baresip -- now that calls go both ways. The older tier list
in [reference-modem.md](reference-modem.md) asked the first questions
(does 14400 work at all, is the echo model right); this is the standing
set that says whether a build is better or worse than the last one, and
where.

Every test here is a call, or a run of calls, that `scripts/bench/sweep.py`
can already place: `one(tag, mode, ms, extra, ref_extra, window, ...,
post=[...], originate=...)`. Where a test needs something the harness
does not do yet, it says so under **Needs**.

## Rules that apply to every test

- **Both directions, always.** A test that runs one way is half a test.
  modec answering and modec calling exercise different start-up code,
  different scramblers, and a different echo path: the CX93001's answer
  tone carries V.25 reversals that stand the ATA's echo canceller down,
  modec's `--ans-plain` tone does not. Every result below is a pair.
- **One variable per run.** The reference is pinned (`AT+MS=<mod>,0,...`)
  unless the test is about negotiation. Error control and compression are
  off at the reference (`AT&K0 AT%C0 AT\N0`) unless the test is about
  them.
- **Repeat what is statistical.** A start-up either works or it does
  not, and the dense rates have shown "works on this call, not the next".
  Handshake results are reported as k of N with N stated, never as one
  call.
- **Score bytes, not CONNECT.** A connection that carries garbage is a
  failure. Payload is indexed lines scored with `ber.score`; binary
  payload is compared byte for byte.
- **Ask the reference what it saw.** Two reports, and which one works
  depends on who placed the call. `AT&V1` after hang-up is the one to
  rely on: termination reason, last and highest transmit and receive
  rate, line quality, receive level, EQM, local and remote retrain counts
  -- for every call. `AT#UD` (keys 20/21 carriers, 26/27 initial rates,
  30-33 losses and retrains, 34/35 final rates, 40-44 error control,
  52-55 characters, 60 termination cause; no SNR, MSE or echo keys on this
  firmware) is filled in only for calls the reference *dialled*; for a
  call it answered it holds keys 0, 1 and 60 and nothing else, in the
  call or after it. `ATW2` makes the reference's CONNECT name the line
  rate. The runner sets `ATW2` and reads both reports on every call.
- **Keep the recordings.** The per-call `recordings/<stamp>-*.wav` and
  `-tx.wav` are never overwritten; the `sweep-<tag>` ones are. Every
  failure is reported with its per-call stamp, because the next step is
  always `modec replay` on it.
- **Record the build.** Every result row carries `git rev-parse HEAD`
  and whether the tree was dirty. A number without a revision cannot be
  compared with anything.
- **Nothing else on the CPU.** No `cabal test`, no replays, while a call
  is up: the audio loop starves and the failure looks like the modem's.

## Pre-flight (before any suite, about 30 s)

| check | how | stop if |
| --- | --- | --- |
| reference answers | `ATI3` on `/dev/modem-ref`, with `CLOCAL` | no `OK` |
| reference is on hook | `ATH`, then `AT` | anything but `OK` |
| ATA's SIP port | `sweep.ata_address()` then `sip_options()` | no 200 OK |
| nothing holds 5060 or 4444 | `ss -ulnp`, `ss -tlnp` | baresip or modec left over |
| build is current | `cabal build exe:modec`; note the revision | build fails |
| PipeWire is up | `pw-cli ls Node` | no daemon |

A failed pre-flight is a bench fault, not a modem result, and the runner
must refuse to write rows until it passes.

## S0. Smoke -- every mode, both ways, once

**Question:** did this change break anything that worked?
**When:** before every commit that touches a modem path. 22 calls, about
30 minutes.

| mode tag | reference | modec | window |
| --- | --- | --- | --- |
| bell103 | `AT+MS=B103,0,300,300` | `--mode bell103` | 40 s |
| v21 | `AT+MS=V21,0,300,300` | `--mode v21` | 40 s |
| bell212a | `AT+MS=B212,0,1200,1200` | `--mode bell212a` | 16 s |
| v22 | `AT+MS=V22,0,1200,1200` | `--mode v22` | 16 s |
| v22bis | `AT+MS=V22B,0,2400,2400` | `--mode v22bis` | 16 s |
| v32-4800 | `AT+MS=V32,0,4800,4800` | `--mode v32 --v32-rate 4800` | 16 s |
| v32-9600 | `AT+MS=V32,0,9600,9600` | `--mode v32 --v32-rate 9600` | 16 s |
| v32b-7200 | `AT+MS=V32B,0,7200,7200` | `--v32-rate 7200` | 16 s |
| v32b-9600 | `AT+MS=V32B,0,9600,9600` | `--v32-rate 9600` | 16 s |
| v32b-12000 | `AT+MS=V32B,0,12000,12000` | `--v32-rate 12000` | 16 s |
| v32b-14400 | `AT+MS=V32B,0,14400,14400` | `--v32-rate 14400` | 16 s |

Payload: the standard 496-byte pair, one direction at a time
(`taketurns=True`), both scored.

**Pass:** both directions intact in both roles. **Baseline
(2026-09-12, e138876):** all pass except modec-calling 14400 (the
reference sends an R3 with no rate).

## S1. Handshake reliability

**Question:** how often does each start-up succeed, and how long does it
take?
**Why:** 12000 has been "0.002 on one call, 0.02 on the next"; one good
call proves nothing about the next.

Every S0 row, N = 10 per direction. The payload is cut to four lines so a
call is short; the test is the start-up.

**Measure per call:** reached CONNECT at each end; seconds from SIP
established to modec's CONNECT and to the reference's; the rate each end
reports (modec's log, `#UD` 34/35); the first `line:` decision error;
retrains in the first 10 s; the modec failure string if any (`no E from
the answering modem`, `no reversal in AC`, ...).

**Pass:** 10/10 at every rate to 9600, both directions. At 12000 and
14400 the result is the rate, with a 90% interval, not a pass.

**Reads as:** a failure clustered in one direction points at that role's
start-up code; the same failure string repeated is a defect; failures
scattered across phases with no pattern are the line.

## S2. Negotiation matrix

**Question:** when the two ends offer different things, do they land
where the Recommendations say, and fail cleanly where nothing is common?
Each case both directions, N = 3.

| # | reference offers | modec offers | must land on |
| --- | --- | --- | --- |
| N1 | `V32B,1,4800,14400` (automode) | `--mode v32bis` | V.32bis, highest rate the line holds |
| N2 | `V32,0,9600,9600` | `--mode v32bis` | V.32 9600 trellis (V.32bis falls back to V.32) |
| N3 | `V32B,1,4800,14400` | `--mode v22bis` | V.22bis 2400 via the reference's ladder |
| N4 | `V22B,0,2400,2400` | `--mode v32bis,v22bis` | V.22bis 2400 |
| N5 | `V22,0,1200,1200` | `--mode v22bis` | V.22 1200 (no S1 from the far end) |
| N6 | `V21,0,300,300` | `--mode v22bis,v22,v21` | V.21 300 |
| N7 | `B103,0,300,300` | default automode | Bell 103 300 |
| N8 | `V32B,1,...` | `--mode v32bis --v8` | as N1, via V.8 (both logs show CM/JM) |
| N9 | `V32B,1,...` | `--mode v22bis --v8` | V.22bis, chosen in V.8, not by timeout |
| N10 | `V32B,0,12000,12000` | `--v32-rate 9600` | no common rate: both clear, no hang |
| N11 | `V22B,0,2400,2400` | `--mode v21` | no common mode: both clear |
| N12 | `V32B,1,4800,9600,4800,14400` (six fields, asymmetric caps) | `--mode v32bis` | observe: V.32bis runs one rate both ways, so what does the pair pick? |
| N13 | `B212,0,1200,1200` | `--mode v22` | observe: modulation is compatible, answer tones are not |

**Measure:** landed standard and rate at each end (`#UD` 20/21 and
34/35 at the reference); time to CONNECT; for N10 and N11, time until
each end reports failure and whether both return to command mode.

**Pass:** "must" rows land exactly there in both roles, in every
repetition. A failure case passes when both ends clear within 60 s and
the next call works. "Observe" rows are recorded, not judged.

## S3. Rate choice symmetry

**Question:** on a clean line, does the reference pick the same rate
whichever end calls?
**Why:** this is the open 14400 question in test form. The answering
modem chooses R3 from what its receiver made of the caller's TRN. When
modec answers at 14400 the reference accepts; when modec calls, it has
twice sent an R3 with no rate. If the asymmetry is modec's calling
transmitter, the reference's choice will sit lower when modec calls
across the whole ladder, not only at the top.

- Reference `AT+MS=V32B,1,4800,14400`, modec `--mode v32bis`, clean
  line. N = 5 per direction.
- Then modec's transmit level in the calling role only: `--amp 0.35,
  0.5, 0.7` (-3, 0, +3 dB). N = 3 each.

**Measure:** `#UD` 26/27 (the rate the reference first chose) and 34/35;
modec's R3 read from its log; the payload both ways.

**Reads as:** the same distribution both ways means the path decides and
14400-calling is marginal line; a lower one when modec calls, moving with
`--amp`, is level; lower and not moving with level points at the
calling transmitter's signal (GPC scrambler, caller TRN), which
`modec replay` of modec's own `-tx.wav` through the answering receiver
can then isolate offline.

## S4. Data integrity and endurance

**Question:** do long transfers arrive byte for byte, and does anything
drift over minutes?

| test | what | duration |
| --- | --- | --- |
| D1 binary | all 256 byte values, shuffled, 16 kB, one direction at a time | 2400, 9600, 14400; both roles |
| D2 duplex | 4.7 kB each way at once | 2400, 9600; both roles |
| D3 soak | indexed lines, continuous, both ways taking turns | 10 minutes at 9600 and at 12000; both roles |
| D4 escape inside data | payload containing `+++` with and without a one-second guard | 9600; both roles |

**Measure:** byte-exact or not; index of the first bad byte and bad
bytes per minute; `#UD` 55 (characters the reference lost -- a non-zero
count there is its buffer, not the line); modec's decision error and
timing offset every 5 s (`--line-every 250`); retrains.

**Pass:** D1 and D3 byte-exact, zero retrains, no upward trend in the
decision error over the call. D4: `+++` inside a stream without guard
time is data at both ends; with the guard it escapes. D2 is recorded
against the known finding that the path does not carry sustained duplex
at 9600 (one direction garbled per call); a clean D2 is news.

**Needs:** binary payload and a byte-exact comparison in `one()`; a
per-direction pace for D1 at 14400 (`&K0` has no flow control).

## S5. Retrain and rate change

**Question:** do retrains and renegotiations started by either end, or by
the line, succeed and keep the call?

| test | how | both roles |
| --- | --- | --- |
| R1 reference retrains | mid-call `+++`, then `ATO1` (Conexant: return online with retrain -- verify on the part first) | yes |
| R2 the line forces it | `--line-snr 40@0,10@20,40@23`: three seconds of 10 dB | yes |
| R3 step down | `--line-snr 40@0,21@15,18@27,15@39`, reference automode | yes (exists as `v32b-stepdown`) |
| R4 step back up | noise down then clear: `40@0,15@15,40@35` | yes |
| R5 one direction only | R3 with `--line-snr-dir rx`, then `tx` | yes |

N = 3 each.

**Measure:** retrain events at each end (modec log, `#UD` 31-33); the
rate before and after; seconds from disturbance to data flowing again;
bytes lost across it; whether the call survived.

**Pass:** R1 and R2 survive with data resuming inside 15 s. R3 steps
down at least once and the payload after the last step is intact. R4
records whether either end ever renegotiates up -- V.32bis permits it,
and modec has never been seen to. R5: the rate follows the direction
that was degraded.

## S6. Call control

**Question:** does every way a call can start and end behave, and can one
modec process run call after call?

| test | how | pass |
| --- | --- | --- |
| C1 reference hangs up | `ATH` mid-data | modec `NO CARRIER` within 5 s, back in command mode |
| C2 modec hangs up | `+++` guard, `ATH` at modec | reference `NO CARRIER` within 5 s |
| C3 DTR drop | reference `AT&D2`, drop DTR (`TIOCMBIC`) mid-data | as C1 |
| C4 SIP hangup | `hangup` sent to baresip directly | both ends clear, modec reports `NO CARRIER` |
| C5 no answer | reference `ATS0=0`; modec dials | modec `NO ANSWER` at its timeout, nothing left ringing |
| C6 no such destination | modec dials a SIP address nothing listens on (the ATA rings FXS 1 for any user part) | `BUSY`, `NO ANSWER` or `NO CARRIER` inside 40 s, not a hang |
| C7 ring count | modec `ATS0=3`, reference dials | three `RING`s before modec answers |
| C8 escape and return | `+++` then `ATO` at each end, payload after | data resumes both ways |
| C9 back to back | one modec process, 5 calls alternating direction, no restart | every call as good as the first |
| C10 caller gives up | reference dials, modec `ATS0=0`, reference aborts with a keypress | modec stops ringing, next call works |
| C11 reference not reset | modec calls (reference answers), then the reference dials modec without `AT&F` | observed: tracks the reference quirk below |

**Needs:** `one()` starts and kills modec per call; C9 needs a variant
that keeps modec and baresip up across calls. C3 needs DTR control in
`atlib.AT`. C6 needs baresip's call-closed reason in the log.

## S7. Error control against the reference

**Question:** does modec's MNP interoperate, and does it settle cleanly
when the reference wants something modec does not have (LAPM, MNP 5)?

| test | reference | modec | must |
| --- | --- | --- | --- |
| E1 | `\N2` (reliable) | `--mnp` | MNP link comes up (class 4 or lower), payload exact |
| E2 | `\N3` (auto-reliable) | `--mnp` | settles on MNP or on none; never hangs (exists as `lapm-v22bis`) |
| E3 | `\N3 %C1` (compression) | `--mnp` | MNP without class 5; payload exact |
| E4 | `\N2` | no `--mnp` | reference gives up or drops to buffered; record which |
| E5 | `\N2` | `--mnp` with R2 noise (10 dB for 3 s) | payload exact after retransmissions |

Both roles, at V.22bis and V.32bis 9600.

**Measure:** `#UD` 40/44 (what the reference negotiated), 42/43 (link
timeouts and NAKs), modec's MNP log; time from CONNECT to the link being
up; payload integrity. **Pass:** as the table; E5 is the one that proves
error control does its job on this path.

## S8. Levels

**Question:** over what range of received level does each rate connect
and hold?
**Why:** modec's AGC and slicer have only met the one level the bench
happens to give.

- modec transmit: `--amp` from 0.1 to 0.7 in 3 dB steps.
- reference transmit: its level register (S91 on Conexant parts, in
  -dBm; verify with `ATS91?` first) from -9 to -30 in 3 dB steps.
- modes: V.22bis, V.32 9600, V.32bis 14400; both roles; N = 1 per step.

**Measure:** connect, payload, `#UD` 10/11 (the reference's received and
transmitted power), modec's decision error. **Pass:** each rate works
over at least 15 dB of the reference's transmit range; the table of edges
is the deliverable.

## S9. The simulator, in the calling direction

**Question:** do the noise and impairment campaigns tell the same story
with modec calling?
**Why:** `ber.py` and `distort.py` have only ever run with modec
answering, and the echo path is not the same the other way.

Rerun the smallest useful subset with `originate=True`: `ber.py` for
`v22bis`, `v32-9600`, `v32b-12000` at three SNRs each (the knee and one
either side, from the existing `ber.csv`), and `distort.py` for `clean`,
`jit-s3`, `slips2`, `carrier15`, `loss`, `echo`.

**Pass:** each calling-direction curve within 2 dB of the answering one.
Where it is not, the difference is a finding; open it with the per-call
recordings.

**Needs:** an `originate` argument threaded through `ber.run` and
`distort.run` into `one()`.

## S10. Answer tone and echo

**Question:** what does the answer tone do to the echo modec has to deal
with, and does it matter to the result?

| test | modec answers with | measure |
| --- | --- | --- |
| A1 | `--ans-plain` (bench default) | echo level at modec, payload at 12000 and 14400 |
| A2 | V.25 reversals (drop `--ans-plain`) | same |
| A3 | `--v8` (ANSam) | same |
| A4 | modec calls (the CX93001's tone) | same |

**Measure:** the echo fit of modec's `-tx.wav` against its received
audio at the logged delay (`echo-to-rest` in dB, as in
reference-modem.md), modec's `echo at N ms` line, and the payload.
N = 3 each. **Reads as:** a tone that stands the ATA's canceller down
should show 5-6 dB more echo, as A4 already did (-17.5 dB against -23);
whether it costs the dense rates bytes is the question.

## S11. Start-up under impairment

**Question:** is the start-up the fragile part, as the 2026-09-11 work
suggested?

The S9 impairments at the level where data mode still works, applied
from the moment the call comes up rather than after CONNECT:
`carrier15`, `clock1`, `jit-s3`, `echo`, `loss`, and noise at the data
knee + 3 dB. V.22bis and V.32bis 9600, both roles, N = 5.

**Pass:** S1's success rate within one failure of the clean line. The
failure strings say which phase breaks.

## S12. Growing the corpus

**Question:** which recordings should become fixtures?

Every suite produces recordings; a few are worth keeping, because a
fixture outlives the bench. Mint when a call is the first of its kind or
shows behaviour a fixture does not yet pin. The obvious gaps now that
modec calls the CX93001:

- `cx93001-answering-*` for Bell 103, V.21, Bell 212A, V.22, V.22bis,
  V.32 9600, V.32bis 7200, 9600 and 12000 (only 4800 exists);
- one S2 fallback (N2 or N5) each way;
- one S5 retrain that survived, each way;
- the no-rate R3 from modec-calling 14400, as `connect: none`.

Before minting, confirm the build before the relevant fix fails the new
fixture, as `cx93001-answering-v32-4800` does -- a fixture both builds pass
pins nothing.

## Results

One file, `recordings/bench/results.csv`, one row per call:

`when, rev, dirty, suite, test, rep, dir (answer/originate), mode, ref_ms,
modec_args, ref_result, modec_result, t_connect_ref, t_connect_modec,
rate_ref (#UD 34/35), rate_modec, retrains_ref (#UD 31-33),
retrains_modec, a_intact/a_lines, b_intact/b_lines, bytes_lost_ref (#UD
55), evm_first, evm_last, fail_string, pass, rec_stamp`

`pass` is the test's own rule from above, computed when the row is
written; rows for "observe" tests leave it blank. A suite's summary is a
table of pass counts by test and direction, set beside the same table
from the last recorded revision.

## The runner

`scripts/bench/suite.py`, built on `sweep.one` for the calls and on
`sweep.Stack` (baresip and one modec process, held across calls) for S6:

    scripts/bench/suite.py preflight
    scripts/bench/suite.py S0                          # smoke
    scripts/bench/suite.py S1 --reps 10                # handshake reliability
    scripts/bench/suite.py S2 S3 --reps 3
    scripts/bench/suite.py S6 --only C1,C9 --dir answer
    scripts/bench/suite.py summary [REV]

Implemented: pre-flight, S0, S1, S2, S3 and S6 (C1-C11). Not yet: S4, S5,
S7-S11, and `compare`. It runs pre-flight before each suite, skips rows
already in `results.csv` for the same clean revision, stops on a bench
fault (the reference not answering `AT`, the ATA not answering OPTIONS,
a leftover baresip or modec) rather than recording it, and prints each
row as it lands. "Dirty" means `src/`, `app/` or `modec.cabal` differ
from the revision; docs and bench scripts do not count, so the harness
can be fixed between runs without invalidating results.

**The bench's baresip answers manually** (`answermode=manual` in
`~/.baresip-bench/accounts`, as in the repo's templates). With
`answermode=auto`, baresip answered every call itself and modec's S0 was
never what decided -- which C5, C7 and C10 exist to test.

## What running it found (2026-09-12, e138876)

**S0:** 20 of 22. Both failures are ones S1 exists to count, not
regressions: modec answering V.32 9600 lost three of eight lines into
modec, and modec calling at 14400 trained this time (it has also been
refused, with an R3 offering no rate) and then modec read none of the
reference's lines while the reference read all of modec's.

**S6, call control:** hang-ups from either end, DTR drop, a SIP-side
hangup, and no answer all behave, both directions, NO CARRIER inside
1.3 s at modec and 2.7 s at the reference. Defects in modec:

- **C7:** `ATS0=3` answers on the first ring. S0 is treated as on or off.
  *Fixed since, not yet re-run on the bench:* the interpreter counts rings
  in S1 and answers on the S0th. *Passed on the bench 2026-09-16.*
- **C8:** `ATO` after modec's own escape returns to data (the payload
  after it is intact) but prints no `CONNECT`. *Fixed since:* `ATO`
  repeats the last `CONNECT`. *Passed on the bench 2026-09-16.*
- modec's stderr is unbuffered and written from two threads, so lines
  arrive interleaved character by character ("modem role Anmsowdeerc");
  the runner matches recording paths rather than sentences because of it.
  *Since made line-buffered.*

- **C9, one modec process across calls:** fails even with the reference
  reset before every call. The first two calls are good in either order
  (answer then originate, or originate then answer); the third and fourth
  fail. C10's call after an abandoned one failed the same way. Something
  in modec survives a call and breaks the one after next -- unexplained,
  and the first thing to take apart. (The first C9 run blamed only the
  reference quirk below; the re-run with resets says there is more.)

Test design faults found and fixed rather than recorded: the HT802V2
rings FXS 1 for any user part, so C6 now dials an address nothing
listens on, and passes; C10 counted RINGs that were already in the pipe
before the caller hung up, and now counts none.

**A reference quirk (C11).** After the CX93001 has *answered* a call, the
next V.22bis call it *dials* fails unless it is reset with `AT&F` first.
`AT&V` shows nothing changed but S0. The reference reports `CONNECT 2400`;
modec's receiver locks through S1 and the scrambled ones at 1200 (decision
error 0.004) and loses the signal within a second of the change to 16
points (0.75), where a call after a reset holds 0.005. Replay reproduces
it, so it is in the audio. Whether the reference's signal is wrong in
that state or modec's receiver should follow it is a question for a
second reference modem (T4.1). Every call the runner places resets the
reference first, except C11, which records it.

**Interrupted, not yet analysed.** The rest of the first pass (S3, then S2
and S1) was cut off nine calls into S3. What landed, raw: with the
reference in automode (`AT+MS=V32B,1,4800,14400`) and modec answering
(`--mode v32bis`), both calls had modec report V.32bis 14400 while the
reference's `AT&V1` said 12000 both ways, and modec received none of the
reference's lines while the reference received all of modec's. With modec
calling the same automode reference, none of seven calls connected
(`&V1`: no rate, receive level 215). Neither is a conclusion yet; both are
where S2 and S3 resume.

## What the second pass found (2026-09-16, bfb363c and 89d58a0)

**C9 was the transmit cushion.** Each time baresip connected or dropped its
streams, pw-cat capture came up about 32 ms short (16 to 37 measured)
while playback kept consuming, and the loop writes one block per block
read. The 100 ms cushion wore away a call at a time, and after the third
event pw-top counted an underrun on modec-tx once a second. `Modec.Cushion`
now writes back what capture lost. `MODEC_IO_STATS=1` logs the audio clock
and the cushion every five seconds. C9 passes both ways, four calls each,
and a probe of eight alternating calls in one process ran with no
underrun at all.

**C7 and C8 pass** on the bench, both fixed earlier offline.

**C11 was not the reference's state.** Through the ATA the far end's
level falls by 6 dB, inside 5 ms, at a random moment on most calls. At
2400 bit/s the slow gain let the carrier loop fit the halved four-point
signal to the inner ring and spin, which is the "loses the signal at the
change to 16 points" C11 recorded. `qrAgcStep` in `Modec.QAM` follows
the step. Replayed, 63 of 73 recorded answer calls hold the line where 50
did, with no payload lost. Live, S1 V.22bis is 10 of 10 both ways.

**Automode, modec calling, was a timing window.** A CX93001 in automode
answers with its tone, 2250 Hz for 3 s, then AC for under a second, then
V.21. modec waited to hear AC before sending AA, and the ATA's 780 ms
round trip put its AA past the window: 10 of 10 failed. A V.32-only caller
now sends AA as soon as the answer tone ends. Three of three connected at
14400. One carried both directions; two lost the reference's lines,
which is the 14400 receive fault below.

**Still open.**

- modec receiving 14400 on calls it placed. A pinned call did read both
  ways this pass, and the automode ones mostly did not. The truth trace
  on the failing S0 recording (20260912T211730) shows a true error of
  0.15 from the first data symbol, and holding the start-up's frequency
  does not help, so it is not the seam fault fixed for the answering
  side.
- The CX93001 sometimes answers modec's R2 at 14400 with an R3 offering
  no rate ("no common rate").
- Once in 30-odd calls the CX93001 sent start + 6 data bits with no stop
  bit and printed 0xFF bytes after CONNECT (probe call 20260916T194532).
  That is the reference, not modec.
- Every V.32 connect and retrain now logs both ends' rate signals
  (`rates:` in the call log). In automode the reference offered 4800 only
  on one call and 14400 on the next, so where a call lands is the
  reference's choice.
- A retrain that cannot finish now ends the call after 20 s rather than
  33 s of silence.

## Order and cost

A call is about 50 s including set-up and teardown.

| suite | calls | time | run |
| --- | --- | --- | --- |
| S0 smoke | 22 | 30 min | every commit touching a modem path |
| S1 handshake | 220 | 3 h | overnight, after start-up changes |
| S2 negotiation | 78 | 70 min | after negotiation or V.8 changes |
| S3 symmetry | 19 | 16 min | now: it is the open question |
| S4 integrity | 16 | 60 min (four are 10-minute soaks) | weekly, and after timing or equaliser changes |
| S5 retrain | 30 | 30 min | after retrain or rate-ladder changes |
| S6 call control | ~20 | 25 min | once, then after Hayes or SIP changes |
| S7 error control | 20 | 20 min | once, then after MNP changes |
| S8 levels | ~90 | 75 min | once; repeat if the AGC changes |
| S9 simulator | ~40 | 1 h (long windows) | once, to close the calling-direction gap |
| S10 echo | 24 | 25 min | once, then after echo canceller changes |
| S11 start-up | 120 | 1.7 h | after start-up changes |

Suggested first pass: pre-flight, S0 to fix the baseline in `results.csv`,
then S3 (the open 14400 question), S6 (never tested at all), S2, and S1
overnight.
