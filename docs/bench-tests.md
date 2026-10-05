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
  different scramblers, and a different echo path. Since 2026-10-05
  both answer tones carry the V.25 reversals that stand the ATA's echo
  canceller down; before that modec answered `--ans-plain` on the
  bench, which did not, and results from then are with the ATA's
  canceller in the path one way. Every result below is a pair.
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
  That includes other sessions on the machine, which nothing here can
  stop: with the load at ten to thirty, modec's blocks took 100 to 450
  ms and every call failed. `sweep.py` therefore starts baresip and
  modec in a systemd scope with a CPU weight a hundred times the
  default (`first_in_line`), which is the most this user may ask for;
  pre-flight says when the machine is busy; and a row is only as good
  as its slow blocks. `scripts/bench/probe.py` prints them, with the
  load, beside each call.

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
| A1 | `--ans-plain` (`BENCH_ANS_PLAIN=1`; the bench default until 2026-10-05) | echo level at modec, payload at 12000 and 14400 |
| A2 | V.25 reversals (the default) | same |
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

## What the third pass found (2026-10-04, from 65386d0)

The question was the open one: 14400 connects and then, too often, one
direction is lost. Twenty calls at 65386d0 on an idle machine set the
baseline -- ten pinned with modec calling, five pinned with modec
answering, five with the reference in automode and modec calling. The
reference read every line modec sent on all twenty. Modec read every
line on fourteen, lost all of them on four, and some on two.

None of the six was the receiver failing to read a line it had been
given. Each cause below was measured on a recording, most of them by
`modec replay --tx`, which is new: a replay regenerates its transmit,
which is not what the echo in the recording is an echo of, so until now
no replay could reproduce a call whose trouble was its own echo. Given
the call's `-tx.wav` as the canceller's reference, it reproduces the
live call to the retrain.

**The loop was losing a race, and the cushion keeper was writing
silence into the carrier.** At 14400 the data pump took 13 to 15 ms of
every 20 ms block. Two thirds of that was the trellis decoder building
each subset's list of points afresh for each of thirty-two branches a
symbol, fifty thousand lists a block; it now reads eight metrics a
symbol from tables, and the pump takes 2 ms. Replay traces are bit for
bit what they were. And capture was arriving in lumps of thirteen
blocks: pw-cat 1.6 writes raw audio through a C stream, a stream into a
pipe is block buffered, and 4096 bytes at 8 kHz is 256 ms. So the loop
had to work through thirteen blocks at 14 ms each in the 256 ms before
the next thirteen. When it could not, no read waited; the keeper, which
stamped every block of a burst with the last arrival it had seen, read
the capture clock high, let its settled level follow, and on catching
up found a "loss" of 12 to 44 ms and wrote that much silence to
playback. Our own echo then came back later by exactly the silence
(`scripts/diag/echolag.py`: 6045 samples before, 6145 after, on a
12.5 ms top-up), the canceller's taps were set for where it had been,
and the receiver went from 28 dB to 17. Four of the six. The keeper now
measures only at arrivals -- samples read before a read that waited,
against the instant it returned -- which cannot read high; a model of a
loop that falls behind is in the suite, and the old keeper adds 370 ms
of silence in it. Capture runs under `stdbuf -o0` and arrives a graph
cycle at a time, which took 250 ms off the round trip besides.

**The canceller ran away once, at 10^20 times the line.** An answering
modem is silent for three seconds between its two conditioning signals.
The first far scan after that silence held a reference that was empty
but for sixteen samples of the second signal's leading edge; nine lags
in ten scored nothing, so the mean was nearly nought and the rival was
nought, a score of nothing much at 38 ms stood alone, the filter was
aimed there, and its taps were read off the same sixteen samples --
each a correlation divided by the energy of a pulse's first rise. A lag
is now scored only where the reference under it holds half what the
fullest window does.

**The far search was too slow for an answerer, and is now a
transform.** It scored a few dozen lags a block because seven thousand
dot products would not fit in one, so a scan took one to three seconds
and two had to agree. A caller has been sending for four seconds before
data and was aimed in time; an answerer starts its second conditioning
signal 2.7 s before data and was not. Through `--ans-plain` the ATA's
own canceller usually takes our echo out -- what arrives in the quiet
window is digital silence -- but on two of the first ten answered calls
it did not, and such a call opened at 16 dB and retrained at three
seconds.
All the lags are now scored at once (`Modec.Xcorr`, three transforms
taken a block apart), from the first conditioning signal on, where
Figure 4 keeps the far end silent and the reflection stands alone: 0.6
against a floor of 0.02. With modec calling, the filter is then trained
by the full step for the rest of that signal, to 20 to 28 dB, and the
receiver opens at 27 to 31 dB instead of climbing to it. With modec
answering and the ATA's canceller half working, what is left is not a
steady path -- the best fixed filter takes out 7 to 12 dB of it -- and
that call is still marginal. Whether subtracting helps is now measured
(the prediction's correlation with the line) rather than assumed from
how much is predicted, so a reflection that the ATA takes away half a
second later is no longer subtracted for the rest of the call.

The machine then stopped being idle -- other sessions, load ten to
thirty for most of the night -- and the calls that failed under it, and
after it, showed the rest: each reproduced from its recording, or
measured on the audio path with no modem in it.

**A receiver that follows the line down reads whatever is left on it.**
Two of the failures were one fault. With our echo taken out, the line
is silent for two seconds while the caller sends its conditioning
signal, and the gain went up sixty decibels after it. The timing
detector divides by the level being tracked; when the answerer's
signal returned it put out its limit until the gain came back down,
and the symbol rate estimate was left 3700 ppm out, from which the
narrowed loop did not return. The receiver read the second TRN at an
error of 0.2, missed E, and handed the data pump a line it had not
trained on. While the echo was still on the line the gain had that to
hold on to and rose 23 dB instead, and the same kick was smaller: this
is the "true error of 0.15 from the first data symbol" this file
listed as open, and that recording (20260912T211730), replayed with
its transmit, now reads 25 to 27 dB from the first symbol.

The other was a lost RTP packet in the middle of a call: 20 ms of
exact zeros (20261005T013747, at 23.34 s). For those 20 ms the
receiver was handed the canceller's prediction of our echo subtracted
from nothing -- a clean V.32 signal twenty decibels down -- and the
equaliser, whose step is divided by the power in its window, trained
on it. The line was the same line afterwards, to the last tap; the
equaliser was not. It read 18 dB where it had read 30, which is over
the freeze, so it stayed, and one line of the reference's eight
arrived.

The receiver now waits through a quiet line (`qrFadeHold`): four
symbols in a row sixteen times under the level, and the gain is held
and nothing adapts until the signal is back and has filled the
equaliser. Three seconds of that in the start-up, half a second in
data. Both recordings are fixtures, each with what modec sent beside
it, and each fails on the build before.

**Playback came up a block short after one 57 ms block**, with capture
still buffered: 20 ms of silence in the carrier and the echo 20 ms
late. Unbuffered, ten calls at a load of fifteen had blocks to 58 ms
and no step in any echo's delay.

**pw-cat was the one client in the graph that was not real-time.** The
machine went nearly idle at the end of a run, and three of that run's
last six calls still lost capture: 43 ms out of the middle of an
answered call at a load of two (20261005T035323), and the receiver could
not read the line after it. The I/O trace has it as one read that
waited 70 ms, after which the capture clock is 43 ms down and stays
there -- four graph cycles that were never delivered. Every other
client of the graph works in a real-time thread, which PipeWire's
library starts and rtkit grants. pw-cat 1.6 has no such thread; it
copies each cycle in its main one, at whatever priority it was started
with, and the graph does not wait for a client. Taken apart with no
modem in it -- a loopback, one pw-cat playing silence into it, another
recording it, every arrival stamped -- at a load of eight to ten and a
quantum of 256 frames: 166 ms of capture lost in 45 s and 160 ms in 40,
a cycle to five cycles at a time; with pw-cat's thread made real-time,
nothing, both times. modec now asks rtkit for that for both pw-cats
each time it starts them (`MakeThreadRealtimeWithPID`, which rtkit
grants for a thread of any process its caller owns), and says so if it
is refused. This, and not PipeWire, is what was losing cycles at a load
of thirty earlier in the night, and the CPU weight the harness gives a
call was the same remedy from further away.

**The graph was not ours.** The softphone path is wired to no device,
and PipeWire runs a graph with no device in it on whichever driver is
going: its dummy driver while the machine is silent, the sound card's
once anything plays. During that run a browser began to play (rtkit's
log has its audio thread at 03:55:27), and from the next call every
arrival was the sound card's: its quantum, and its crystal, 16 ppm off
the machine's clock where the calls before show none. Then a mixer
window was opened. Its level meters ask a few milliseconds' latency of
every node they watch, ours among them, and the quantum went from 512
frames to 256 in the middle of a measurement; that run lost 19 ms of
capture where the runs either side of it lost none, and a meter
attached on purpose, 256 to 128, cost 3 ms at the instant it attached.
Two properties on our nodes now: `node.group = pipewire.dummy`, the
dummy driver's own group, which keeps them on it whatever the sound
card is doing, and `node.force-quantum = 960`, which nothing linked to
them can ask its way past. 960 frames of the graph's 48 kHz is the
loop's 20 ms block, so capture arrives a block at a time. With the
browser playing, the mixer open and a load of nineteen, the capture
clock of a 50 s call stayed within 0.1 ms of where it began. The
softphone's streams joining still costs a cycle on some calls, 20 ms
where it was 9, before either modem has said anything.

**modec was cutting its own line.** With the mixer open, five calls of
eleven heard nothing or were not heard. Snapshots of the graph through
them: the link from the line to the softphone's input gone, or the one
from the softphone's output to us. The softphone's capture sometimes
gets the default microphone linked in beside the line, and modec takes
such links out when a call comes up -- any link into a node the line
feeds, by the node's name. A mixer's meter on the line is a node the
line feeds, and every meter the mixer has open goes by the mixer's
name, so the link into each of them from the sound card, the browser
and everything else was a stray: sixty in one call. They were removed
by link id, each twice because the listing shows a link at both its
ends, and PipeWire gives a freed id to the next object made (remove
link 186, make a link, it is link 186) -- which, as a call comes up, is
one of the softphone's. The rule goes by port now, and a link is
removed once, named by the two ports it joins. None of this was in any
earlier run because no mixer was open in any earlier run.

**A gain hit, and a receiver with no rule for one.** With those three
in, the machine went quiet and twenty calls were taken: nineteen clean.
The twentieth (20261005T050643, modec answering) connected, read the
line at an error of 0.001 for eight seconds, and then could not read it
at all. The recording has the far end's level doubling at 20.95 s and
halving again 80 ms later -- four packets -- with the symbol clock
running straight through; the same recording with those 641 samples
halved reads through them at the error it had before. It is the ATA,
which has stepped levels by 6 dB since the first V.22bis call through
it. The V.32 receiver's gain is slow on purpose and had no rule for a
step: it went 3 dB after the hit, took half a second to come back, and
for all of that every point landed on another. At 14400 a decision is
never far from some point, so nothing that guards the loops saw
anything wrong, and they trained on it. V.22bis's rule -- hold the loops
when the last dozen symbols' power leaves the run before them, take the
new level outright once it has lasted -- is now on every V.32 rate with
the slow gain. The recording is a fixture. Without the rule it retrains
two seconds after the hit, at a pinned rate with nothing to retrain to;
with it the receiver is reading a quarter of a second after, and the
reference's eight lines, which the live call never saw, all arrive.

**What the fixtures said.** Three had been failing since the TRN went
from 1400 to 4096 symbols. A replay regenerates its transmit, and a
recording's far end is answering the transmit of the day; with a
longer TRN the replay was still sending when the far end had moved on.
A fixture now says which TRN it was recorded under (`trn:`), and two
of the three pass as they were. The third pinned three hundred bytes
of a far end's last half second, a pattern repeating every six bits
that does not decode to one thing; it and its twin stop at 31 s now,
with every byte before that unchanged. And a fixture can carry what
modec sent on the call (`NAME.sent`), which is what lets a bench call
with its echo be one at all.

**Where it stands.** The baseline was fourteen of twenty. With the
audio path mended the machine went quiet -- a load of one to four -- and
stayed in use as a desktop, a browser playing and a mixer open, and two
runs of twenty were taken: nineteen clean, and nineteen clean. Placing
calls at a pinned 14400, sixteen of sixteen. Placing them against the
reference's automode, eight of eight, each at 14400. Answering with the
plain tone, fourteen of sixteen. One of those two was the gain hit,
mended between the runs. The other is what the plain tone has always
risked: the ATA's canceller left in charge of our echo and not taking
it out (20261005T052950 -- the reflection at 525 ms, a decision error
of 0.0088 from the first symbol, and modec's canceller, aimed at a path
that was not steady, not helping).

So S10's A2 was run, which this file had listed as untried since
modec's canceller could reach the reflection. Answering with the V.25
reversals, which stand the ATA's canceller down for the call: twenty
four of twenty four, eight lines each way on every one, no retrain.
The reflection is there on each -- 0.5 to 1.2 % of the line, at 495 to
545 ms -- modec's canceller is on, and the receiver's error is 0.0014
at the median of 120 reports. `--ans-plain` was the bench's answer for
as long as modec's canceller could not do that; the harness answers
with the reversals now, and `BENCH_ANS_PLAIN=1` is the old way.

Under load, where the night's calls came apart: three of three at a
load of seventeen once pw-cat was real-time, with capture not a
millisecond short.

And S0 on the committed code (2c200d9), every mode both ways once, as
the check that none of this cost the other ten modes: twenty of twenty
two. Answering, all eleven, the six V.32 rates with the reversals.
Calling, nine of eleven, and neither miss is the receiver this pass
worked on. V.32 at 9600 uncoded lost seven characters of one line. The
recording loses the same seven with this pass's receiver changes and
without them, there is no hole and no hit under them, and that rate
reads the line at 0.003 to 0.007 with moments of 0.02 where 14400 reads
it at 0.001: the sixteen-point receiver's own loops, which were short a
line in September too. V.21 calling: the reference read one line of
eight and misframed the rest, after our echo came back 32 samples
sooner from one second to the next -- 4 ms, a bit and a fifth at 300
bit/s -- with nothing in modec's log. V.21 calling was then run six
more times, six clean, no step in any.

**Still open.**

- Lost packets: 20 to 60 ms of the received audio gone, once every few
  calls. They no longer cost the call, and still cost the line or two
  of an unprotected payload that was in them. The path, and what error
  control is for. The same goes for a gain hit, which costs a quarter
  of a second.
- A pinned call cannot retrain: the offer below the pinned rate is
  empty, so a receiver that loses the line stays lost. Right for the
  bench, where the rate is the test; wrong anywhere else.
- The canceller's on-and-off rule let go once in the twenty-four
  answered calls, for one report, with the reflection where it had
  been; the error went to 0.0073 while it was off and the call lost
  nothing. At 14400 an uncancelled reflection at 0.6 % of the line is
  most of the margin, so the rule wants a longer look before it lets
  go.
- A start-up that fails grows the heap while it waits: by the time a
  caller gives up on an answerer it cannot hear, forty seconds in, a
  block every few seconds takes 160 ms. It costs nothing a failing
  call had left, and is not right.
- The softphone's streams joining still costs a graph cycle on some
  calls -- three of forty, 20 ms each, before either modem has spoken.
- The echo's delay still steps on one call in fifty with nothing in
  modec's log: by 80 samples, which is one of the ATA's 10 ms frames
  and is in September's recordings as often, and once by 32 the other
  way, the V.21 call above, which is in none of the 380 calls looked
  at before it and whose origin is not known. At 14400 a 10 ms slip is
  twenty-four symbols and eighteen turns of the carrier exactly, and
  the receiver rides through it; but the canceller's taps are then
  10 ms out, and for the seconds it takes the search to find the
  reflection again the receiver is at 0.0086, which at 14400 is not
  reading (S0's 14400 call, after its payload). The search should be
  told at once when a canceller that was helping stops.
- V.32 at 9600 uncoded, as above: the loops of the sixteen-point
  receiver put more into the decision than the line does.
- The CX93001 sent five-bit characters for a whole call once, the first
  call after the host was rebooted (20261005T012108). The reference,
  as before.
- The no-rate R3 was seen once in thirty-odd calls, under load, on a
  call whose start-up had blocks of 72 ms in it, and not in the ninety
  since. It may always have been holes in our TRN: the
  quiet-window echo search that this pass took out of live calls cost
  most of every block for exactly as long as the TRN lasted.

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
