# Testing modec against a reference modem

A hardware modem is the one thing this project has never had: an
implementation of the same Recommendations that was not written here. It
is worth more as an *oracle* than as a peer. Every test below is chosen
because it answers something modec cannot answer about itself.

Two facts shape the whole list:

- **Every fixture in `test/fixtures/live/` is `role: originate`.** The
  corpus has never contained a call modec answered, because
  `Corpus.replayCase` replays fixed audio and cannot answer a modem that
  speaks first. A reference modem dialling *in* is the only source.
- **The open failure is narrow and well located.** V.32bis connects at
  12000 and 14400 and then does not deliver the payload. The trained
  data pump is *not* the cause: swept offline it reads 14400 cleanly at
  26 dB SNR and above, 12000 at 20 dB and above, which is where theory
  puts them. Nor is it noise, level, the byte gate, or the constellation
  tables — see "What has been ruled out". What is left is the handoff
  from the start-up to the data pump, and a reference modem is the way
  to see which side of it is wrong.

## What has been ruled out

Worth having on the bench so no session re-runs it:

| Suspect | Verdict | Evidence |
|---|---|---|
| Noise or arriving level | no | identical results from SNR 30 to 46 dB, and 0 to −20 dB loss |
| The trained data pump | no | offline sweep clean at ≥26 dB (14400), ≥20 dB (12000) |
| The byte gate (`mcMaxEvmV32`) | no | raising it to 1e9 changes nothing; slicer error is 0.003 |
| Constellation labelling | no | uncoded bits *and* trellis subsets are fully invariant under 90/180/270° |
| Echo | no | present in none of these runs |

The failure is deterministic per noise seed and unchanged by more SNR,
which is the signature of an acquisition outcome rather than an error
process: one seed yields zero bytes on every run, another yields 42–49
bytes for a 35-byte payload on every run.

## 0a. Driving the bench: start the modem with the call, not before

The first thing that will waste an hour. Over the SIP path, modec must
be told about the call, not merely pointed at the audio:

```
modec modem --answer --sip 127.0.0.1:4444 --audio-sip-loop modec --hayes ...
```

with `ATS0=1` on the data side. `--sip` drives baresip over its
`ctrl_tcp` module, so RING and ATA are real events and the modem starts
at the instant the call is answered.

Point modec at `--audio-sip-loop` *without* `--sip` and it starts
listening the moment the process does, which is ten seconds or more
before the ATA's INVITE arrives. V.22bis survives that -- its answering
side waits indefinitely -- so the path looks fine. V.32 does not: its
start-up is timed, and it gives up on a line that is still silent, with
`connection failed: no calling modem` and a receive recording of
digital zeroes. That reads exactly like broken audio routing and is
not: check `pw-link -l` and you will find `baresip:output ->
sip-to-modec` and `modec-line -> baresip:input` both correctly linked.

## 0. Topologies, and why you want both

**A — through the ATA (SIP path).** Modem → ATA FXS → SIP → baresip →
modec. This is the path modec actually ships on. It has G.711, a jitter
buffer, and almost no near-end echo, because the ATA's hybrid is short
and clean.

**B — the two-wire loop (analogue path).** Modem and modec's transformer
across one battery-fed pair, per `docs/line-interface.md` Option A/B. No
codec, no packetisation — and a *real* hybrid mismatch, so a real
near-end echo.

The difference between A and B is the point. B is the only way to test
echo on hardware, and A is the only way to test what ships. Run the
critical tests on both and compare.

On the ATA, before anything else: G.711 only, echo canceller **off**,
VAD/silence suppression **off**, fixed jitter buffer, fax/T.38 detection
**off**, 0 dB gain both ways. Every one of those defaults is wrong for
data.

## 1. Verify the modem itself (5 minutes, do it first)

```
AT&F                    factory defaults
ATI3                    firmware / chipset
AT+MS=?                 the modulation menu -- authoritative
AT+FCLASS=?             1 = fax, 8 = voice available?
```

**Pass:** `+MS=?` lists `B103`, `B212`, `V22`, `V22B`, `V32`, `V32B`.
If `V32B` is absent, most of this list is unreachable and you have the
wrong unit.

Set a known state for everything that follows:

```
AT&F
AT\N0                   no error correction (raw modulation)
AT%C0                   no compression
AT&K0                   no flow control
ATS0=0                  do not auto-answer yet
```

`\N0` matters more than it looks: with V.42/MNP left on you are testing
the reference modem's error correction, not modec's modulation. On
modec's side there is nothing to turn off: MNP is opt-in with `--mnp`,
and `--no-mnp` is not an option the `modem` subcommand has.

## The first sweep, 2026-09-10

Every modulation the two implementations share, one call each, modec
answering over SIP through an HT802V2, the reference a CX93001 pinned
with `AT+MS=<mod>,0,<rate>,<rate>` and `AT\N0 AT%C0` so nothing but the
modulation is under test.  A 496-byte pattern went modec to reference
and a 464-byte one came back, compared byte for byte.

| Standard | Rate | modec -> reference | reference -> modec |
| --- | --- | --- | --- |
| Bell 103 | 300 | clean (cut short by the harness) | **clean** 464/464 |
| V.21 | 300 | clean (cut short by the harness) | **clean** 464/464 |
| Bell 212A | 1200 | **clean** 496/496 | **clean** 464/464 |
| V.22 | 1200 | **clean** 496/496 | **clean** 464/464 |
| V.22bis | 2400 | **clean** 496/496 | **clean** 464/464 |
| V.32 | 4800 | no connection | no connection |
| V.32 | 9600 | no connection | no connection |
| V.32bis | 7200 | `no rate signal R2` | — |
| V.32bis | 9600 | **clean** 496/496 | **clean** 464/464 |
| V.32bis | 12000 | 103 B of rubbish | nothing |
| V.32bis | 14400 | 113 B of rubbish | nothing |

The 300 bit/s rows are not failures: 496 bytes at 300 bit/s needs 16.5
s and the harness read for 16, so what arrived was a clean prefix.
Two rows needed a second run to settle -- V.22 showed one 8-byte burst
at offset 287 and was clean on repeat, and V.32bis 9600 showed one
corrupt receive and was clean on repeat -- so treat both as clean with
an intermittent line event, and neither as a modulation fault.

**What this answers.** The 12000 and 14400 failure is *not* modec
talking to itself. Against a second implementation it fails the same
way it fails in loopback, and modec says why:

```
CONNECT V32bis 14400 bit/s, 1800 Hz both ways, Answer, trellis coded
retraining the link: this receiver could not read the line
connection failed: no common rate
NO CARRIER
```

It trains, cannot read what it trained on, asks for a retrain, and the
retrain finds no common rate. So the "connects and delivers nothing"
symptom is real, reproducible against hardware, and modec's own
diagnosis of it points at the receive side -- while the transmit side
is now proven good, because a Conexant read 496 bytes of modec's
V.32bis 9600 without an error.

**Newly open.** `--mode v32` (as against `v32bis`) never reached
CONNECT at either 4800 or 9600, and logged *nothing at all* after `SIP
call up, modem role Answer` -- a silent failure path worth a message
before it is worth a fix. V.32bis at 7200 got as far as `no rate
signal R2`.

**Also settled here.** Bell 212A carried data both ways, against a
genuine Bell answerer, which `bbslist.txt` records as never having been
achieved against any of the twelve boards that were tried. T2.2 is done.

### The second batch, same day

Re-run with a 24 s read window, Bell 103 and V.21 are clean both ways
(496/496, 464/464), so every row from 300 to 2400 bit/s now stands
without a footnote. Six of these calls are in `test/fixtures/live/` as
`cx93001-*`, the first `role: answer` fixtures in the corpus.

| Test | Reference | modec | Result |
| --- | --- | --- | --- |
| V.8, T2.3 | `AT+MS=V32B,1,4800,14400` | `--v8` | **V.8 works**: modec's answering CM/JM exchange landed `CONNECT V32bis 14400` against a real menu -- then the 14400 receiver fault, as everywhere |
| automode, no V.8 | same | no `--v8` | connected 14400, retrained, **`now running at 12000 bit/s`** -- T3.2's rate renegotiation, seen for the first time -- then the 12000 receiver fault |
| LAPM, T3.4 | `AT\N3` at V.22bis | `--mnp` | **fails the criterion**: 19 s after CONNECT, `MNP link down: no reply to the link request`, and the call drops |
| guard tone, T3.5 | `AT&G2` at V.22bis, as caller | -- | first call timed out in `AV22Ones2400`; **repeat was clean both ways**. Not a guard-tone effect: a caller does not send one, and 1800 Hz is barely in the audio (peak 0.02). T3.5 proper needs the reference answering, which is blocked (below) |

**Intermittent, and counted.** Three calls out of about twenty on
otherwise clean modes failed once and passed on repeat: an 8-byte
burst in V.22, a corrupt receive at V.32bis 9600, a handshake timeout
at V.22bis. Roughly one call in seven. Nothing yet says whether that
is the SIP leg, the ATA, or modec; the recordings of the failed ones
are kept, and the answer is a job for the channel simulator once one
of them has been read.

The LAPM one is worth the words. `Modec.Mnp` implements A.7.2.2
faithfully: link requests unanswered *and* damaged frames seen means
there is a protocol over there, so it sends LD to say goodbye. A LAPM
modem's XID and SABME are exactly "damaged frames" to an MNP framer,
and a V.42 modem that receives an MNP disconnect hangs up. So the pair
never reaches the "no error correction" outcome the option promises;
the disconnect modec sends is what ends the call. The far end in `\N3`
would have fallen back to plain mode on its own if modec had simply
gone quiet. That is a fix, not a finding to live with.

### What the failures are, read offline

Every failed start-up above is reproducible from its recording with
`modec replay --answer`, which turns a bench afternoon into a unit:

- **Plain V.32 (`--mode v32`, 4800 and 9600) is not silent, it is
  slow.** Replay: AR1 at 16.5 s, ATrainR2 at 18.7, ACond2 at 21.6,
  **AR3 at 22.3, and nothing after.** AR3 in `Modec.V32Start` has two
  exits: signal E, or 80000 symbols later `no E from the calling
  modem`. A plain V.32 caller sends R2, reads R3, and goes to data --
  there is no E in V.32, only in V.32bis §5.3.2. The Conexant pinned
  to `V32` does exactly that, modec waits 33 s for an E that cannot
  come, and the reference's S7 gives up first. **modec cannot answer a
  V.32 modem that is not a V.32bis modem.** Loopback never showed it
  because both ends were modec, and modec always sends E.
- **7200 is the opposite: R2 is there and unread.** `v32trace` on the
  recording finds `rate signal 7200 tcm (V.32bis)` from the reference
  at 17.36 s, after modec's own R1 of the same at 16.48; replay sits
  in ATrainR2 until 51.96 s and fails `no rate signal R2`. At 9600 the
  same modem sends a plain-V.32 R2 (`rate signal 9600`) and modec
  connects; at 12000 and 14400 it sends V.32bis R2s and modec
  connects. What is different about 7200's is not yet known, and the
  recording is `recordings/20260910T173045-1001.wav`.
- **On a real modem's idle, this receiver emits junk.** At V.32 9600,
  after the payload and before the far end hung up, ~500 spurious
  bytes over 18 s of scrambled ones. The 9600 fixture is cut at 16.5 s
  to keep that out of the reference decode.

### The four, worked (2026-09-10 evening)

**LAPM interworking: fixed.** The first diagnosis was wrong and the
recording says so. `AT\N3` is *auto-reliable*, not LAPM-only: the
CX93001 sends one perfectly well formed MNP link request --

```
16 10 02 17 01 02 01 06 01 00 00 00 00 ff 02 01 03 03 01 08 04 02 40 00 08 01 02 10 03 0c 18
   ^DLE STX  len  LR ...                                          ^DLE ETX  ^CRC
```

-- waits its own establishment timer, gives up, and passes the DTE's
data through in the clear. `Modec.Mnp` saw that one frame, latched
`msSawFrame`, and thereby disabled the data detection that would have
noticed the 464 bytes of plain ASCII arriving next; it went on offering
MNP to a far end that had stopped listening, and at the end of its
retries sent the disconnect that killed a working call. The latch is
now a recency test -- a protocol that is still there frames something
inside its own timer -- and the same call gives `no error correction:
the far end did not answer` at 16.9 s and then carries the payload
clean both ways. Sending LD now also needs a well formed frame, not
merely a damaged one, so a genuine LAPM-only peer gets silence rather
than a disconnect it would honour.

**14400: it is not the start-up, and it is not the tables.** Two
measurements bound it hard.

`--v32-rate 9600t` -- trellis coded, 32 points, Table 3/V.32 -- carries
496 and 464 bytes without an error against the CX93001. That single
result exonerates the whole shared path: the trellis encoder, the
Viterbi decoder, the differential quadrant coding, the echo canceller,
and above all the start-up-to-data-pump handoff that this document has
been pointing at since the fork. 9600 trellis goes through every one of
them.

And the constellations are structurally sound. Counts and uniqueness
are right; the lattices are what the module claims (odd/odd for 16, 64
and the 7200 set, the checkerboard for 32 and 128); mean powers are
10, 10, 10, 42 and 41 as documented; and every one of the four
trellis sets partitions into **8 subsets with a 9.0 dB partition gain**,
12000 and 14400 included. Whatever is wrong is not a mistyped point and
not a broken Ungerboeck partition.

(The guess that followed here -- that the uncoded-bit labelling was
the remaining suspect -- did not survive the Recommendations; see
"Read against the Recommendations" below.)

**7200 fails earlier, and differently.** `scripts/diag/v32bits.hs`
prints the descrambled bits the rate-signal detector is reading. At
9600t, 12000 and 4800 they are the random-looking stream a converging
receiver produces. At 7200 they are `1010101010101010`, unchanging, for
the whole 33 s of `ATrainR2` -- a constant alternation, not a signal.
The receiver never locks onto the caller's TRN at all, so R2 is on the
line (`v32trace` finds `rate signal 7200 tcm (V.32bis)` at 17.36 s) and
cannot be read. (The lattice was not it; the answerer was starting
to train two seconds late, and the caller had given up. Below.)

**Plain V.32 stalled in AR3** -- replaying either recording reaches
`AR3` at 22.3 s and never leaves. The reading at the time was that a
V.32 caller might send no E. It does (§5.3.2/V.32); the fault was the
anchor `detectE` demanded. Below.

### Read against the Recommendations (2026-09-10, night)

V.32 (03/93) and V.32bis (02/91) were fetched from the ITU and read
rather than remembered, and the four were re-examined against them.
Everything static in the V.32bis path checks out, item by item:

- **Signal E is in V.32 too.** §5.3.2/V.32: "any rate signal other
  than R1" ends with one E; Figure 4/V.32 shows `R2 | E | B1` on the
  call side. The "plain V.32 sends no E" reading above was wrong.
- **Figures 2-1, 2-2, 2-3 and 2-4/V.32bis are `points128`, `points64`,
  `points32` and `points16at7200` exactly** -- every label extracted
  from the figures' text layer with its coordinates and diffed, 128/128,
  64/64, 32/32, 16/16, the method validated on the 9600 set that
  hardware had already passed.
- All four trellis sets partition into 8 subsets at 9.0 dB; the uncoded
  bits are invariant under every rotation, and the subset permutation
  under rotation is the same one in all four sets.
- Bits per symbol, uncoded bits, and the packing order `Y0 Y1 Y2 Q3 ..`
  match §2.3 and the figures' labels for every rate.
- Levels: the figures draw A, B, C, D at (±6, ±2) for 12000 and 14400
  and (±3, ±1) for the rest, against set mean powers of 42, 41 and 10,
  so training and data sit within 0.2 dB at every rate -- which is
  what normalising both to unit power assumes.
- Clocks: modec's carrier measures 1800.003 Hz and its symbol rate
  2400.0000 Bd off its own transmit recording (§2.1: ±1 Hz, ±0.01 %);
  the reference arrives at +0.1 Hz and +47 ppm through the ATA's clock
  chain, inside spec both.

**1. Plain V.32 -- fixed, and it was the anchor.** With E in V.32, the
bit dumps in `AR3` show the caller's R2 repeating with about one error
in every sixteen to thirty-two bits, and `detectE` required the sixteen
bits before E to decode exactly and equal the R2 read earlier. E comes
once. The anchor now tolerates two bits, which leaves it eleven from an
E and eight from any other sequence's structure. All three stalled
recordings connect in replay, and on the bench 4800 (both ways of
pinning it) and 9600 non-trellis now connect and carry data.

**3. 7200 -- fixed, and it was MT.** §6.2 has the answerer cease on the
caller's S, wait MT, then train on the S that persists. MT is there so
the answerer's own echo of R1 has died before its receiver restarts,
and the text assumes an MT of tens of milliseconds against a TRN of at
least 533. Here MT measures two seconds, so modec trained 1.3 s after
the caller's TRN had ended, on whatever it was sending by then, and
lived on how long the caller kept repeating R2. A CX93001 pinned to
7200 gave up 40 ms after modec finally started listening. The wait is
now capped at 512 symbols (213 ms) -- inside TRN on any terrestrial
path, with the taps ACond adapted covering a real echo -- and **7200
connects and carries the payload both ways.** The cap also moves every
other start-up's training into TRN where it belongs.

**4. 12000 and 14400 -- it is the echo, and it is quantified.** Nothing
in the coding is wrong; see the list above. What the bench does have is
a reflection of modec's own transmission off the ATA's hybrid,
returning through the softphone **1159 ms later at −28 dB** (found by
correlating the transmitted answer tone with what came back: the
"hum" first blamed for a 23 dB tone SNR was modec's own 2100 Hz,
delayed). The far end arrives 8 dB below modec's transmit level, so the
uncancelled echo sits 20 dB under the wanted signal, predicting a
decision-error floor of 0.010; every V.32 call on the bench measures
0.012 to 0.020 regardless of rate. Against the offline thresholds --
32-point trellis about 18 dB, 12000 about 20, 14400 about 26 -- that
floor carries 9600t marginally (the one-in-seven flakes), 9600
non-trellis worse (no coding gain: corrupt receive twice), and 12000 and
14400 not at all. The reference modem's own canceller cannot reach a
1.2 s echo any more than modec's could, which is why the failure was
symmetric.

`Modec.Echo` searches 500 ms for its echo, because voip.ms had put one
at 116 ms, and it stays at 500 ms. Two ways of reaching this one were
tried tonight and both withdrawn on live evidence. A full-resolution
1.5 s search, run every block until something is found, starved the
real-time loop: three start-ups in a row lost at R2. Letting the search
run while the far end talked aimed at **85 ms** instead of 1159,
cancelled nothing, and a filter adapting at the wrong delay against a
signal it cannot predict took 9600t's decision error from 0.015 to
0.033. A cheaper wide search confined to the quiet windows -- a stride-four
grid with a refinement -- was clean on one live 9600t call and then
failed three loopback tests in the suite: 7200 text both ways, 12000
and 14400, and the classic ladder, whose calling side lost R3. The
simulator's echo is close and the coarse grid does something to it
that the live path never showed. It was committed on a summary line
read too quickly and reverted the same hour; disproven, not merely
withdrawn. The shape of the real fix is clear and is not
a bench evening: on a path this late the reflection of the aperiodic
TRN never arrives inside a quiet window, so the canceller would have to
be aimed and trained in data mode, decision-directed, after the
start-up.

(And a third harness lesson: the sweep overwrites `sweep-<tag>-rx.wav`
on every run of a tag. Replay the per-call recordings under
`recordings/`, which are never overwritten, when the question is what
a change did.)

**The fix was one bit in the answer tone.** The ATA's line echo
canceller was on all along -- and standing down for every V.32 call,
because modec's answer tone carries V.25's phase reversals every
450 ms, and G.164/G.165 have every canceller on the path switch itself
off when it hears 2100 Hz reversed like that, on the understanding
that a modem sending it will cancel its own echo. Through a softphone
that understanding is wrong: the one canceller close enough to the
hybrid to matter is the ATA's. `--ans-plain` sends the tone without
the reversals. Measured, same evening, same bench: the reflection of
the answer tone goes from **−27.6 dB at 1140 ms to −53 dB with no
correlation at all**; 9600t sits at 0.009–0.016; **12000 connects and
carries the payload both ways at a decision error of 0.005–0.006**,
about 22.5 dB, for the first time on any hardware -- and again on a
second call at 0.010-0.011, payload delivered both ways, then a
retrain once the far end went idle, so it is real and it is marginal;
14400 connects and still cannot read the line.

The thresholds quoted earlier in this file were wrong, and the survey
run the same night (`modec-bench v32-survey`, `recordings/bench/`) has
the real ones for the trained pump on an ideal telephone channel:
**9600t reads at 16 dB and fails at 14; 12000 at 20 and fails at 16;
14400 at 22 and fails at 20** (52 errors in four thousand bits). So
the bench's ~22.5 dB is not hopeless for 14400, it is the edge -- and
the same survey says what pushes it over: every dense rate dies on the
mildest timing jitter it offers (sine, 1 sample at 2 Hz: 14400 loses
1680 bits in 4000, 12000 loses 933), where 9600t rides through.

So the bench's own impairments were put into the simulator
(`modec-bench v32-bench`: G.711 mu-law, the far clock 47 ppm fast, the
carrier 0.7 Hz off, on top of the noise) and they do not reproduce it:
14400 reads through all of them together down to 22 dB, and only at 20
does the combination cost more than the ideal channel (1457 errors
against 52). Nor is it timing wander on the live path -- measured off
the recordings it is 0.04 to 0.08 samples RMS, ten to twenty times
below the mildest jitter the survey offers. What the offline harness
does not exercise is the one thing left: it trains the pump itself,
where a live call hands the pump the equaliser and carrier the real
start-up converged against a real far end. At 9600t and 12000 that
handoff is good enough; at 14400 the margin it leaves is what is
missing, by a decibel or two.

**Caught, with `--line-every 12`.** In the second between CONNECT and
the retrain, 14400's decision error is **0.008 to 0.010** -- about
20.5 dB, between the survey's "reads at 22" and "fails at 20" -- and
12000's is 0.004 to 0.007. The receiver was reading 14400, marginally.
What declared it unreadable is the retrain gate: `mcMaxEvmV32` is
**0.5**, not the 1.0 quoted earlier in this file (only the replay
command ever exposed it), and the gate is the square root of the
error power against half the rate's decision margin -- 0.078 at 14400,
so an error power of 0.0061. Every block above that counts as unread,
and a second of them is a local retrain. At 12000 the same gate is
0.0119, which the first call cleared and the second call's 0.0168
excursion breached -- its "could not read the line" once the far end
went idle. At 9600t it is 0.025, never approached.

That reading lasted twenty minutes. With the gate raised to 0.7 and
0.8 (the live modem has the option now, `--max-evm-v32`), 14400 holds
for the whole call with no retrain, and **modec's 14400 transmission
reaches the reference byte-perfect, 496/496, twice** -- the
transmitter is proven against real hardware. But what modec received
was not a payload with a few errors: 10,689 bytes for 464 sent, edit
distance 460 of 464, random. The 0.5 gate was refusing a link that was
in fact unreadable, and the default stays at 0.5.

Is the receiver even locked? Computed, not assumed: a receiver whose
points are scattered like the signal itself reports a nearest-point
error power of 0.037 on the 128-point grid, 0.056 on the 64, 0.074 on
the 32. Live: 0.009, 0.005, 0.015. So at 14400 the symbols are mostly
resolved -- the equaliser and carrier are doing their job -- and the
bits are random anyway: the loopback's "clean symbols, wrong bytes",
now on hardware, and one-directional, because **modec's 14400
transmission reads clean at the reference.** At 20.5 dB of white
noise the offline pump reads 14400 with a bit in a hundred wrong;
random output is not that. It looks like a trellis decoder that has lost its
path and does not find it again. What does that on a real line and not
in the survey is not yet known: the survey's group-delay rows do not
single 14400 out (1 ms is fine for 9600t, 12000 and 14400 alike, 2 ms
breaks all three), so it is not ISI as the simulator models it. And the receiver's quality is not a property of the line: three
12000 calls an hour apart on the same path ran at 0.005, 0.010 and
0.012-0.031 -- six decibels between the best and the worst -- and the
worst, with the gate raised to 0.7, passed bits from a receiver below
12000's own threshold and delivered nothing readable, where the 0.5
gate had withheld them. What differs call to call is the state the
start-up hands the pump. Sent one line at a time with idle between, so the framer can find
its alignment again after a bad bit: **12000 delivers all eight lines
intact**, and **14400 delivers one of eight** -- 610 consecutive
correct bits, which no receiver decoding at random produces -- with
ten kilobytes of wrong characters around it. So the 14400 receiver is
marginal and not systematically wrong: it holds the trellis path for
stretches and loses it, and what it needs is margin, not a correction.
What the receiver lacks is the thing §5.4.2 hands it for free: B1, 128
symbols of scrambled ones at the data rate and coding, a known sequence
at the data constellation. This receiver waits for 64 ones before
arming the framer and does not adapt on them.

**And that is worth about two decibels, measured.** `modec-bench
v32-aided` runs the same signal and the same noise through the trained
pump twice: once decision-directed, once handed the symbols the far end
actually sent. No real receiver has those -- the point is to measure
the ceiling before building the machinery to reconstruct them.

| rate | SNR | decision-directed | data-aided |
| --- | --- | --- | --- |
| 14400 | 22 dB | 0 errors | 0 |
| 14400 | 20 dB | **52 errors** | **0** |
| 14400 | 18 dB | **1153 errors** | **439** |
| 12000 | 18 dB | 12 errors | 12, EVM 0.0107 -> 0.0094 |

So training on truth moves 14400's threshold from "fails at 20" to
"clean at 20", and the settled decision error at 18 dB from 0.0093 to
0.0070. That is the margin the bench is short of: it measured about
20.5 dB effective where 14400 wants 22. At 9600t and at high SNR there
is nothing in it, which is the expected shape -- a receiver that is
already right has nothing to learn from being told it is right.

`Modec.QAM` now has the hook: `qamRxRef` queues points the far end is
known to have sent, and while the queue lasts the carrier loop and the
equaliser train against those instead of against the receiver's own
decisions. The decision error keeps its old meaning -- everything above
that module is asking the same question it was -- and with an empty
queue the receiver is bit for bit what it always was, which the whole
suite checks.

**What remains is reconstructing the symbols on air, and the oracle
harness has already found the traps.** Three of them cost an hour each
and all three apply to the real thing: the reference has to be aligned
in time (the channel's band-pass has group delay, so the data does not
begin where the sample count says -- an eight-symbol search found
nothing, at a cost per symbol indistinguishable from noise, and it took
sixty-four); it has to be in the receiver's frame (V.32 is
differentially encoded because absolute phase is unknowable, so the
receiver settles into the constellation turned by some quarter turn,
and possibly mirrored, and a real data-aided receiver must resolve that
before it can use anything it predicts); and where the reference is
absent it must fall back to the decision, because padding with the
origin trains the equaliser towards zero and destroys it. The
prediction itself is the easy part: the far end's scrambler register is
the line-bit history this end already holds, its convolutional encoder
starts from zero at E, and B1's input is all ones -- so every point is
computable without any feedback from a noisy decision.

One of the flakes showed its mechanism on the way: a call where
baresip's RTP stayed at `audio=0/0` for seven seconds after the SIP
answer. The caller's AA was on the line and never reached modec, and
the start-up ended `no answer`. When a call fails to train at all,
look there before looking at the modem. What is left between 22.5
and the ~30 dB the line's noise floor allows is receiver implementation
loss -- timing and carrier jitter, equaliser misadjustment -- and that
is the next thing, and a modec thing.

**Two lessons for the harness.** Never run the test suite while a bench
call is in progress: the real-time audio path starves and every
start-up fails at R2, which looked like a regression for twenty
minutes. And the recordings begin at process start, not at the call:
the call arrives around 11 s in, and modec's answer tone runs to 14 s.

### T3.1 and T3.2 on a line that gets worse (2026-09-10, late)

`modec modem --line-snr` puts noise on a live call. `Modec.Channel`
does this to a recording, whole-signal, which a live call cannot use --
a band-pass restarted every twenty milliseconds splatters at every seam
and one seed repeats the same noise for ever -- so what comes over is
the one impairment that decides whether a rate can be held: additive
noise, which is stateless per sample. The convention is the simulator's
exactly (sigma is the signal's r.m.s. over 10^(snr/20)), so a number
here and a number in `modec-bench v32-survey` mean the same thing.
`--line-snr 40@0,21@15,18@27,15@39` is a schedule counted from the call
coming up; `--line-snr-dir rx|tx|both` chooses whether this end hears
it, the far end does, or both.

**The ladder works.** With the reference in automode and the line
walked down through the survey's thresholds:

```
CONNECT V32bis 14400 ... retraining: this receiver could not read the line
now running at 12000 ... retraining ... now running at 9600
```

and with both directions of the reference capped at 12000, so that
nothing but the noise is driving it, **12000 -> 9600 -> 4800 with the
payload delivered both ways**. That is T3.2, the least-tested path in
the codebase, working against real hardware for the first time. T3.1's
other half turned up too: `retraining the link: the far end asked` --
a far-end-initiated retrain, which until now only `modemDuplexDisturb`
had ever produced.

**7200 is reachable, and is never chosen by a renegotiation.** Capped
at 7200 the pair connects `V32bis 7200`, carries the payload both ways,
and steps down to 4800 under noise -- so both ends can run it and modec
can select it. But in five step-downs the chosen rates were 12000, 9600
and 4800 and never 7200, including the decisive case: with the
reference offering exactly 9600, 7200 and 4800, the step from 9600 went
straight to **4800**.

`ratesBelow` is not what drops it -- it clears by bit rate, so 7200
survives a step below 9600 -- and `allV32Rates` prefers 7200 over 4800.
What can rule it out is `bestCommonRate`'s guard: 7200 and 12000 are
V.32bis-only, and `bis` requires *both* rate sequences to announce
V.32bis. If the far end's renegotiation signal clears B4, the usable
set is V.32's alone -- 9600 trellis, 9600, 4800 -- and below 9600 only
4800 is left, which is exactly what happens. That is the leading
explanation and it is not yet proven: nothing logs the peer's rate
sequence during a renegotiation, and it should. Until it does, "the far
end proposed 4800" is not excluded.

**Two more things the noise turned up.** A retrain that cannot
re-establish sits for about 33 seconds before giving up -- the
start-up's per-phase timeout -- during which the call is silent and the
DTE is told nothing; on a stepping call that is most of a minute of
nothing happening. And a link that had just stepped to 4800 at 13 dB
went `NO CARRIER` a second later, where the survey says 4800 rides down
to 10; what ended it is not yet known.

### Error rate against noise, both ways, every mode (2026-09-11)

`modec modem` can now put the channel simulator on a live call.
`Modec.Channel.Live` is the same `Channel` -- the same record, the same
profiles, the same `--impair K=V` -- run a block at a time with its
state carried across blocks: filter histories, delay lines, the random
walk's last value, the loss chain's state and last good frame, an
impulse's ring-down into the next block, the running level the noise is
set against. `--line-snr` sets the noise on a schedule counted from the
call coming up; `--impair` and `--channel` set everything else;
`--line-snr-dir` chooses which direction hears it.

Three things could not come over from the whole-signal version and are
worth knowing. A filter that was centred is causal here and arrives
late by half its length, which `liveLatency` reports and no modem can
see. A resampler cannot make samples before they exist, so a clock
offset is a delay that starts at a second and drifts from there. And
every warp of time is *biased* rather than clamped: sine jitter swings
its delay down to zero, and a floor put under it does not delay the
signal, it flattens the bottom of every cycle into a different
waveform -- which is a distortion the caller did not ask for, and it
was in the first version of this module.

**The method.** `scripts/bench/ber.py` places one call per mode per
signal-to-noise ratio. The reference dials in pinned by `AT+MS` with
error control and compression off (`AT&K0 AT%C0 AT\N0 AT%E0`), so
nothing between the two modems corrects anything: what is measured is
the modulation and the receiver, not a retransmission protocol. The
handshake runs clean at 40 dB; the noise steps in two seconds after
that mode's CONNECT, and the payload goes three seconds later still, so
every byte of every payload crosses a line already at the stated ratio.
The payload is 24 indexed lines of 62 varied printable characters --
about 1500 characters, which resolves a character error rate down to
roughly 7e-4 -- scored line by line: a line is found by its index and
compared character for character, and counted lost if its index never
arrives. `modec --max-evm-v32 3` opens the byte gate so that wrong bits
are delivered rather than withheld, since wrong bits are the
measurement.

Two mistakes in the first attempt are worth recording, because both
made the numbers look better than they were. The noise stepped in at a
fixed thirteen seconds, which is after CONNECT for V.32 but well into
the payload for the modes that connect in five -- so the first quarter
of those payloads crossed a clean line, which leaves the character
error rate about right, a waterfall that steep moves a tenth of a
decibel, and the count of intact lines badly wrong, because the intact
ones were the ones sent before the noise arrived. And the payloads went
both ways at once: with about 4.7 kB in flight in each direction, two
clean-line calls in a row each had *one* direction demodulate to
garbage, and which direction it was flipped between runs. Sustained
full duplex is more than this path carries. Each direction is now
measured on its own, which also takes the near end's echo of its own
transmission out of the measurement.

**The comparison.** `scripts/bench/theory.py` gives the textbook error
rate for each modulation on the bench's own definition of
signal-to-noise -- white noise over the whole 0-4 kHz band, sigma the
signal's r.m.s. over 10^(snr/20). A receiver's matched filter keeps
only the noise in its own band, so

    Eb/N0 [dB] = SNR [dB] + 10 log10(4000 / bit rate)

which is the whole reason 300 bit/s FSK reads at 0 dB and 14400 does
not. The curves are the standard AWGN results: non-coherent binary FSK
for Bell 103 and V.21; coherent 4-PSK with a factor of two for
differential decoding for Bell 212A and V.22; square-QAM with a factor
of 1.5 where two bits of four are carried in a differentially coded
quadrant, for V.22bis and V.32 9600; and for the trellis rates the
uncoded constellation carrying the same payload plus 3 dB, which is the
usual figure for the 8-state Ungerboeck code and no better than a rule
of thumb. Character error rate is taken as ten times the bit error
rate -- one wrong bit per wrong ten-bit character -- which is a floor:
a start-stop framer does worse than that on both modems, because one
wrong bit can misalign the characters after it.

**What it measured.** The threshold below is where a direction
collapses -- past 20 % of characters wrong, or nothing arriving at all
-- rather than where it crosses 1 %, because the bench contributes
occasional bursts of its own and one damaged line in twenty-four is
already 4e-2. Those bursts are real and worth knowing about: at 4 dB on
a link whose textbook error rate is 3e-8, two lines came back short and
scrambled, and a six-line burst hit Bell 212A at 40 dB. They are
intermittent -- the same points ran clean on a repeat -- so a character
error rate below about 5e-2 on a single call is at the measurement
floor, and only the dB axis is worth reading.

| mode | rate | theory | reference reads modec | modec reads reference |
| --- | --- | --- | --- | --- |
| Bell 103 | 300 | -2.3 | -1.4 (+0.9) | -1.7 (+0.6) |
| V.21 | 300 | -2.3 | -2.0 (+0.3) | -1.3 (+1.0) |
| Bell 212A | 1200 | 0.0 | held to 2 | 2.4 (+2.4) |
| V.22 | 1200 | 0.0 | held to 2 | 5.7 (+5.7) |
| V.22bis | 2400 | 6.2 | 7.5 (+1.2) | 9.6 (+3.3) |
| V.32 4800 | 4800 | 6.0 | 7.0 (+1.0) | 9.1 (+3.1) |
| V.32 9600 | 9600 | 12.3 | 14.5 (+2.2) | 21.4 (+9.2) |
| V.32 9600 trellis | 9600 | 8.7 | 12.2 (+3.6) | 18.2 (+9.6) |
| V.32bis 7200 | 7200 | 5.6 | 8.2 (+2.7) | 13.7 (+8.1) |
| V.32bis 12000 | 12000 | 11.6 | never | never |
| V.32bis 14400 | 14400 | 14.5 | 27.6 (+13.1) | never |

**modec transmits at very nearly the textbook.** Every mode from 300
bit/s to 9600 is read by the reference within **0.3 to 3.6 dB** of the
theoretical curve, and that figure is not modec's alone -- it contains
the reference's own receiver and everything the ATA, the codec and the
softphone do on the way. The two 1200 bit/s modes never collapsed at
all inside the range tried. This is the strongest evidence yet that the
transmit side is right, and it agrees with the narrower result from the
14400 work: modec's 14400 transmission reaches the reference
byte-perfect.

**modec receives 0.6 to 3.3 dB behind at 4800 and below, and 8 to 10 dB
behind above it.** The break is sharp and it is not gradual: FSK is
within a decibel, V.22bis and V.32 4800 within about three, and then
7200, 9600 and 9600 trellis are all eight to ten decibels adrift, with
12000 and 14400 never carrying a byte in either direction at any
signal-to-noise ratio tried.

**And the decision error says exactly why.** On a *clean* line -- 40 dB,
where the noise is nothing -- modec's V.32 receiver reports these
floors, against what each constellation needs to be read at all:

| rate | points | slicer SNR needed | floor measured | margin |
| --- | --- | --- | --- | --- |
| 4800 | 4 | ~10 dB | 18.4 dB | +8 |
| V.32bis 7200 | 16 | ~17 dB | 18.1 dB | +1 |
| 9600 trellis | 32 | ~20 dB | 20.8 dB | +1 |
| 9600 | 16 | ~19 dB | 22.0 dB | +3 |
| 12000 | 64 | ~23 dB | 17.0 dB | **-6** |
| 14400 | 128 | ~26 dB | 20.2 dB | **-6** |

That floor was taken for the hybrid echo of the earlier section -- the
1159 ms reflection `--ans-plain` removed -- and it is not quite that:
the section after this one measures it, and it is a *second*
reflection, of the data rather than the tone, at 644 ms, that the
canceller could not reach and, until it was made to work in data mode,
did not try to. It is a ceiling on the whole V.32 family that no
amount of signal-to-noise can lift. 4800 has eight decibels of
margin over it and works. 7200 and 9600 trellis have one, which is why
they read on a clean line and collapse as soon as any noise eats into
it. 12000 and 14400 are six decibels *under* the floor before the line
is touched, which is why they never worked tonight and why no
signal-to-noise ratio in the sweep made any difference to them. The
receiver is not failing at 12000 and 14400 because the line is noisy;
it is failing because its own echo is louder than the constellation is
fine.

So the answer to "how does modec compare" is two answers. Transmitting,
it is within a few decibels of theory at every rate it offers.
Receiving, it is within a few decibels up to 4800 and then runs into a
floor of its own making, and every rate that needs more than about
20 dB of slicer is beyond it on this bench until the canceller can
reach that reflection -- which is the data-aided work the previous
section measured at about 2 dB, and the decision-directed aiming that
section leaves as the next design change.

### The higher rates, debugged: it is echo, and not the one that was fixed (2026-09-11)

The campaign above left 12000 and 14400 under a floor -- a decision
error of 0.01 to 0.02 on a clean line, 17 to 20 dB of slicer, where a
64-point constellation needs 23 and a 128-point one 26 -- and the
section before it had already removed an echo. So the first thing to
check was whether the floor was echo at all, and that is a measurement,
not an inference: cross-correlate what modec transmitted against what it
received, in a window where its own data was going out, and look for
where the transmission reappears. The far end's signal is uncorrelated
with ours, so a correlation peak is a reflection, with its delay and
its level.

Done in the wrong window first. The bench recordings start when the
process does, some eight seconds before the call, and the window I took
for "data" held the start-up: the transmit spectrum had lines at 600,
1800 and 3000 Hz with ten-decibel troughs between, which is the S
signal's -- two states alternating -- and a periodic reference gives a
correlator a confident answer at a delay set by arithmetic. The
per-call recordings start at the call, and the per-call log says where
CONNECT is; the windows below are a few seconds into data mode, after
each end's payload, while it idles on scrambled ones.

| call | reflection | gain re our transmit | under the far signal |
| --- | --- | --- | --- |
| 12000 at 40 dB | 644 ms | -34.0 dB | -20.4 dB |
| 14400 at 30 dB | 651 ms | -35.6 dB | -22.1 dB |
| 9600 trellis at 40 dB | 673 ms | -41.7 dB | -27.9 dB |
| 4800 at 40 dB | 658 ms | -34.5 dB | -20.6 dB |

**A reflection 20 to 22 dB under the far end's signal is an error
power of 0.006 to 0.01, which is the floor.** It sits at 644 ms in
every two-second window of a call -- eight windows, not a sample of
drift -- so it is one place on the line and it does not move. And in
the same call the answer tone reflects at -60 dB, which is nothing:
`--ans-plain` did what it was for, the ATA's canceller converged on
2100 Hz and cancels it, and then, with both modems transmitting for
the rest of the call, its double-talk detector froze it there, and the
wideband reflection of our data goes past it 34 dB down.

Nothing on modec's side could touch it, and the code said so in three
places. `Modec.Echo` searched 500 ms back; the reflection is at 644.
It searched only in the start-up's quiet windows, on the principle --
correct for a canceller's usual step -- that with both ends
transmitting the far end's signal enters the error term and drives the
filter off the echo path. And in data mode the modem called it with
adaptation off, every block, for the length of the call. So every V.32
rate that needs more than about 20 dB of slicer was under a floor no
signal-to-noise ratio could lift, which is precisely what the campaign
measured.

**The fix, in `Modec.Echo`: a canceller that works while the far end
talks.** In data mode the modem now calls `echoBlockData`. An
incremental search ('Scan') scores a slice of lags each block against
a two-second window, band-limited to the modem's own 600-3000 Hz, and
reaches 900 ms; the filter is aimed on a plurality -- two of the last
three scans' best lags agreeing for a first aim, three of four to move
one -- because on a real call the reflection wins the scan two times in
three and a noise peak wins the third, somewhere different each time.
Once aimed, the taps come from the scan itself: the correlation of the
two windows over the taps around the peak, scaled by the one factor
that minimises the residual over the window, averaged scan to scan,
read off a dozen taps a block. A slow normalised update holds them. The
filter switches on by its own rule -- the share of the line it predicts
-- since the old rule compared residual against received power and
cannot trigger while the far end is most of what is received.

Four things went wrong on the way and each is in the code's comments,
because each is a fact about a real-time canceller rather than a slip.
Estimating all 256 taps from one window is worse than nothing: each tap
carries the far end's signal as noise, a tenth of the main tap, and 256
of those sum to five decibels above the echo; only the taps around the
peak, averaged across scans, come out below it. A correlation divided
by the reference's energy is the response through the reference's own
autocorrelation, whose in-band gain is the sample rate over the
bandwidth, three and a third; unscaled, the filter predicted three
times the echo. The scores were consed onto a list nothing read until
the scan finished, so all seven thousand were evaluated in that one
block: 336 to 370 ms against a budget of 20, seventeen blocks of audio
gone, which the far end read as junk and answered with a retrain, and
which looked for two hours like the canceller degrading the
transmitter. And the switch-on threshold sat a coin toss above the
estimate's own prediction, since the estimate runs at about half the
echo. A block-time report in the live loop (`slow block: N ms`) is what
found the third; a trace of the canceller's state in the line report
(`canceller on predicting 0.28%`) is what found the fourth.

**What it does.** On the recorded 12000 call whose floor was 0.02: aimed
at 662 ms nine seconds into data mode, on four seconds later, and the
reflection in its output down from -35 dB to -44 dB by then and -47.5 dB
a few seconds after -- nine to twelve decibels of the echo gone, into
the noise. Live, it aims on every call it has been given (672 to
690 ms; the jitter buffer sits differently each time), the loop's
worst block is back to the pump's own 30 to 37 ms, and the far end
reads clean bytes. A loopback test now puts a -20 dB reflection 644 ms
late under a 12000 bit/s call with both ends talking and expects the
text whole both ways, and the modem passes it.

**What it does not do, yet, and why.** The 14400 decision error did not
move on the call where the canceller aimed and converged, and that
call's recording says why: its echo is -39 to -41 dB, some 25 dB under
the far end's signal -- a third of the 0.009 floor at most -- and the
rest is the receiver's own loss on a hundred and twenty-eight points.
The same session gave 12000 its best call of the night, 0.002 to
0.004 with the canceller never aiming, and its worst, 0.02, a minute
apart on the same line: the state the start-up hands the pump varies
by a decade call to call, and on a bad handoff the echo is the smaller
half of the floor. So the echo is out of the way and was not the whole
of it. What is left is in two places -- the handoff, and the loops on a
dense grid -- and the data-aided training the section before measured
at two decibels is aimed at the first.

Two things the block timer turned up belong here too. Even with the
canceller off, the live loop's worst block at 14400 is 33 to 39 ms,
nearly twice the budget, and a 12000 call has blocks of 20 to 30: the
pump itself runs close to the edge, which is a plausible source of the
bursts the campaign saw and a target on its own. And the reference,
with its own retrain monitor left on (`AT%E0` was set only in ber.py),
asks for a retrain about 27 seconds into every 12000 call, on either
setting of the canceller: its judgement of modec's transmission, not
of the line.

### Every other impairment, live (2026-09-11)

`scripts/bench/distort.py` puts one impairment at a time on both
directions of a call, from the first ring, with no added noise, and
scores the same payload each way. Where the offline survey sweeps the
same axis the value is the survey's. *clean* is every line intact;
*nothing* is a call that connected and carried no readable line;
*no link* is a call that never reached CONNECT at both ends. Read each
cell as "the reference reading modec / modec reading the reference".

| condition | V.22bis: ref / modec | V.32 9600: ref / modec | 9600 trellis: ref / modec | 12000: ref / modec | V.21: ref / modec |
|---|---|---|---|---|---|
| clean `` | clean / clean | clean / 1e-03 | clean / 5e-01 (21 lost) | clean / nothing | clean / clean |
| jit-s1 `sinejit=1` | nothing / clean | no link / no link | no link / no link | no link / no link | · / · |
| jit-s3 `sinejit=3` | nothing / nothing | no link / no link | no link / no link | no link / no link | clean / clean |
| jit-walk `jitter=0.5` | no link / no link | no link / no link | no link / no link | no link / no link | · / · |
| slips2 `slips=2` | 3e-01 (14 lost) / nothing | no link / no link | nothing / nothing | no link / no link | 3e-02 (6 lost) / 3e-03 |
| wow `wow=0.3` | nothing / clean | no link / no link | no link / no link | no link / no link | clean / clean |
| flutter `flutter=0.1` | clean / nothing | clean / 1e-01 (12 lost) | clean / nothing | clean / nothing | · / · |
| clock1 `rate=0.01` | 8e-01 (23 lost) / 1e-01 (6 lost) | no link / no link | no link / no link | no link / no link | · / · |
| carrier15 `freq=15` | clean / clean | no link / no link | nothing / clean | nothing / clean | no link / no link |
| delay2 `delaydist=2` | no link / no link | no link / no link | no link / no link | no link / no link | · / · |
| wobble `wobble=2` | nothing / no link | 3e-01 (15 lost) / 2e-02 (5 lost) | 4e-01 (19 lost) / nothing | nothing / nothing | · / · |
| phasejit `phasejit=10` | clean / clean | clean / 3e-01 (13 lost) | clean / clean | clean / nothing | · / · |
| softclip `softclip=3` | clean / clean | no link / no link | no link / no link | no link / no link | · / · |
| harm2 `harm2=0.1` | clean / clean | clean / 1e-03 | clean / clean | clean / clean | · / · |
| hum `hum=0.03` | clean / clean | clean / 6e-02 (8 lost) | clean / clean | clean / clean | nothing / clean |
| impulse `impulse=5` | clean / 8e-02 (16 lost) | 5e-02 (1 lost) / 1e-01 (15 lost) | 4e-02 (2 lost) / 1e-01 (11 lost) | 9e-02 (5 lost) / nothing | clean / 1e-01 (8 lost) |
| hits `hits=2` | 1e-01 (5 lost) / 2e-01 (15 lost) | 1e-01 (2 lost) / 2e-01 (17 lost) | 4e-02 (3 lost) / 5e-02 (3 lost) | no link / no link | · / · |
| dropout `dropout=0.02` | 8e-02 (4 lost) / 2e-01 (15 lost) | no link / no link | no link / no link | no link / no link | · / · |
| biterr `ulaw=1 biterr=0.001` | nothing / no link | 3e-02 (2 lost) / 2e-01 (11 lost) | 2e-02 / 5e-02 (15 lost) | 2e-02 (1 lost) / 1e-01 (14 lost) | · / · |
| loss `loss=0.02` | 5e-02 (8 lost) / 3e-01 (17 lost) | no link / no link | no link / no link | no link / no link | 6e-01 (18 lost) / 2e-01 (10 lost) |
| echo `echo=0.2` | 2e-02 / nothing | no link / no link | no link / no link | no link / no link | · / · |

Three things stand out, and none of them is what the survey said.

**The V.32 start-up is the fragile part, not the data pump.** The
survey said the trained pump reads through 1 % clock offset, 15 Hz of
carrier, 1 ms of delay distortion and light soft clipping. Live, from
the first ring, every V.32 rate fails to *connect* under sine jitter,
a random walk, wow, 1 % clock, 2 ms of delay distortion, soft clipping,
dropouts and frame loss -- the start-up ends in `no common rate` or
never completes -- where V.22bis and V.21 connect and carry data under
most of the same. The survey never exercised the start-up. That is the
next thing to measure offline, and the impairment schedule that
`--line-snr` already has (clean through the handshake, then the
impairment) is how to separate the two live.

**The two receivers fail on different timing impairments.** With the
same warp on both directions, ±1 sample of sine jitter at 2 Hz and
0.3 % wow at 1 Hz leave the reference reading *nothing* of modec while
modec reads the reference clean; 0.1 % flutter at 25 Hz does the
reverse at every rate. A slow, wide warp is inside modec's timing loop
and outside the Conexant's; a fast one is the other way round. The
same at 15 Hz of carrier offset -- twice what V.32 §2.1 requires -- and
at 2 Hz of carrier wobble: modec's carrier loop follows, the
reference's does not. Those are design choices in loop bandwidth, and
modec's are the wider ones.

**Transients and the digital span hurt modec more.** Impulse noise,
gain hits, dropouts, G.711 bit errors and frame loss all cost modec two
to five times the reference's character error rate, with many more
lines lost -- and a lost line here is the start-stop framer losing its
alignment and not finding it again for a while. The reference recovers
character alignment after a burst faster than modec does. That is the
framer, not the modem, and it is cheap to fix.

The whole of it is on the page, beside the noise curves.

### Iterating the receiver against the simulator (2026-09-11)

Three things came out of asking whether the offline harness could stand
in for the bench.

**The loopback could not have reproduced any of it.** `Modec.Loopback`
applied the line with `applyChannel` per block and a fresh seed each
time: fine for noise, meaningless for anything with memory. A band-pass
restarted every twenty milliseconds splatters at every seam; a warp can
only read inside its own block; a 1 % clock offset is a splice every
block rather than a drift. It runs through `Modec.Channel.Live` now,
with the state carried across blocks, and `modec-bench v32-startup`
puts the live matrix's own conditions through a full call, start-up
included. It reproduces the bench: `jit-walk`, `delay2`, `clock1`,
`slips2`, `hits`, `loss` and `echo` fail offline as they failed live,
and `carrier15` reads clean at 9600 trellis and 12000 in both. The
survey never exercised the start-up, which is why it had said these
were survivable.

One axis is already diagnosed. Against 2 ms of group delay distortion
-- what a loaded subscriber loop has -- the trained pump fails at
1400 training symbols and reads **zero errors at 4000**, at 0.002 and
at 0.006 of LMS step alike:

| rate | delay | training symbols | step | errors | settled EVM |
| --- | --- | --- | --- | --- | --- |
| 9600 trellis | 2 ms | 1400 | 0.002 | 1740 | 0.0363 |
| 9600 trellis | 2 ms | 4000 | 0.002 | **0** | 0.0039 |
| 9600 trellis | 2 ms | 1400 | 0.006 | 1732 | 0.0443 |
| 12000 | 2 ms | 4000 | 0.006 | **0** | 0.0011 |

The equaliser needs more symbols, not a faster step. What a real call
gives it is TRN's length and then B1, and that is the connection to the
next item.

**B1 training, and why it must check itself.** §5.4.2 puts 128 symbol
intervals of scrambled ones between the far end's E and its data, at
the agreed rate and coding. They are predictable -- the scrambler is
self-synchronising, so its register is the last 23 line bits and this
end has them -- so `aidB1` re-encodes the far end's E from that
register, and the coder state it leaves encodes 128 ones into the
points the far end put on the line. Two unknowns stand in the way and
both are settled by evidence rather than arithmetic: the quarter turn
the receiver happens to hold, and where E sits among the received
points, which the bit detector's offset gives only to within a few
symbols. The predicted E is searched for near where the offset says, at
every turn; a real match is unmistakable, 1e-5 per symbol against 2 for
anything else.

And then the prediction is checked before it is used, against the B1
symbols the far end has *already* sent. That is not belt and braces. A
false E match -- the search landing where the register does not
correspond -- predicted B1 that disagreed with what arrived **0 of 57
symbols one call and 57 of 57 the next**, and injecting those points
made the handoff worse than leaving it alone: 0.021 where
decision-directed read 0.0001.

**It works, and it is off.** The prediction is exact -- nine symbols of
B1 out of nine, at an E-match cost seventy times under threshold, on
every call. Getting there took three rounds of "it makes no
difference", all of them the same self-inflicted bug: the corroboration
gate asked for sixteen symbols of agreement when E is recognised about
*nine* symbols into B1, so nine is all there ever is, and a perfect
prediction was discarded silently every time. Reachable, it engages.
What it does not do is help:

| 12000 bit/s | errors per seed, decision-directed | trained on B1 |
| --- | --- | --- |
| 20 dB | 58, 0, 0, 59, 91, 57 | 54, 0, 0, 53, 94, **0** |
| 18 dB | 7007, 259, 8478, 483, 213, 999 | 7007, **1234**, 8478, 342, **328**, 999 |
| 16 dB | 1790, 8560, 1914, 1940, 7640, 2008 | 2027, 8560, 1865, 1838, 7640, 1986 |

A gain at 20 dB, a wash at 16, and at 18 dB it is worse on two seeds of
six and better on one. The reference is already stopped short of B1's
end -- queueing the whole remainder ran it into the far end's data and
was worse again, 259 errors becoming 971 -- and that fixed the overrun
without turning the result positive. So `mcAidB1` is off, `--aid-b1`
turns it on, and what the section above called a two-decibel ceiling
from the oracle remains unclaimed by any real receiver. The oracle knew
the symbols with no prediction, no alignment and no turn to resolve;
this knows them exactly and still cannot spend them, which says the
handoff's variance is not mostly in what B1 could teach.

**The framer never recovered from a lost bit, and that is most of the
burst damage.** A start bit is a space that follows a mark. Taking any
space as a start bit is right until it is wrong: one bit lost from the
stream and the framer latches onto a data zero, reads the next
character's start bit as a bad stop bit, drops it, hunts again from a
data bit, and latches onto the next data zero. Simulated against the
old rule: after a single deleted bit it **never realigned in twenty
trials of twenty**, and put out a steady stream of bytes with the high
bit set -- which is exactly what every burst on the bench was followed
by, on both modems' outputs, for lines at a time. Insisting on the edge
it realigns every time, within about fifteen characters. That is the
2-to-5x in the impulse, hits, dropout, bit-error and frame-loss rows of
the live matrix, and it was nine characters of Haskell.

### The cause, measured: the carrier loop loses the frequency on a dense grid (2026-09-12)

Everything above read the receiver through its own `decision error`,
the squared distance to the *nearest* point. On sixty-four or a hundred
and twenty-eight points that number cannot see the operating point: once
the true error is comparable to a decision cell it stops growing, at
about d²/6 -- **0.0159 at 12000 and 0.0081 at 14400**. Every "marginal"
14400 reading of 0.008-0.012 and every dead 12000 call at 0.02 sat at or
above that ceiling, which means they could have been 20 dB or 5 dB and
the metric would have said the same. The offline pump at an 18 dB input
reports 0.0093 too (the aided table above). So the first job was a
measurement that does not saturate.

**The truth trace.** `modec replay --truth` predicts the far end's
symbols and reports the error against them. Not from E and B1 -- that
attempt is written up below -- but from the data pump's own decoder:
once its descrambler and trellis decoder are in step, the descrambler's
register is the far scrambler's, the last Y1 Y2 it decoded is the far
differential encoder's memory, and the Viterbi path fixes the far
convolutional encoder's state, all in the receiver's own frame. From
those the far end's scrambled ones are encoded exactly as it encodes
them, queued (`Modec.QAM.qamRxTruth`) and checked symbol by symbol;
four misses in a row drop the queue (the far end has data to send, or
the receiver has lost it) and the next block arms it again. So the true
error is available whenever the far end idles, for as long as it
idles, from the first block the decoder is in step -- and on the bench
the reference idles on ones for seconds after CONNECT and between
lines. Beside it the trace prints the share of ones among the
descrambled bits, which needs no reference at all: a far end with
nothing to say sends ones, a descrambled bit is one when its three
line bits came through right, so on an idle line 100 % is a receiver
in step and 50 % is one decoding noise, whatever it reports.

**What it found**, on the recordings this file has been arguing over
(all replayed as the answering modem, `--max-evm-v32 3`; "true" is the
mean squared error against the predicted point over half a second of
idle; the loop figures are the replayed receiver's own carrier
frequency estimate, in hertz, as the start-up ends and data mode
begins):

| call | reported | ones | true | receiver | freq: R2, seam, +0.5 s, +1 s |
| --- | --- | --- | --- | --- | --- |
| 12000 `-ec` (the good one) | 0.003 | 100 % | 0.0028, 25.5 dB | locked | +0.1, +0.2, -0.1, -0.1 |
| 12000 `-noec` | 0.008 | 100 % | 0.0088, 20.5 dB | locked | +0.1, +0.3, +0.5, +0.3 |
| 12000 `ber` 40 dB (dead, flat 0.02) | 0.020 | 50-58 % | 0.3-0.4, 5 dB | **spinning** | +0.1, +0.7, -1.3, **-3.1** |
| 14400 `-g08` | 0.009 | 50-53 % | 0.2-2, under 7 dB | spinning | +0.1, -0.8, -1.5, +0.3 |
| 14400 `235126` (211 s) | 0.009 | 50-54 % | 0.2-2, under 7 dB | spinning | +0.1, -1.8, +0.2, -2.2 |
| 14400 `032546` | 0.009 | 50-54 % | 0.2-2 | spinning | |
| 14400 `031609` | 0.003 | 100 % | 0.0025, 26 dB | locked, 8/8 lines | |
| 12000 `042805` | 0.005 | 100 % | 0.0051, 22.9 dB | locked | |

Two regimes and nothing in between. A locked receiver reads the far end
at 20 to 26 dB with the true error equal to the reported one; a dead
one has a true error of 0.2 to 0.4 -- five to seven decibels, the
constellation not being read at all -- while reporting 0.01 to 0.02,
which is the nearest-point ceiling and nothing else. The "14400 is
marginal at 20.5 dB" of the sections above was the ceiling talking.
And 14400 is not always dead: `031609` carries all eight lines at 26 dB.

What the dead receivers are doing is visible on the stretches where
the prediction held for a few hundred symbols: within any hundred
symbols the points fit the truth to 0.01-0.03 after one complex
scalar, and that scalar's angle swings by ±30 degrees from one hundred
symbols to the next. The equaliser is not it -- its taps, printed at
the handover and a second and twenty seconds later, are the same
filter on a good call and a dead one, and skipping the 256-symbol
acquisition window changes nothing. Timing is not it: `sps` sits at
3.3333 throughout. Level is not it: the received level falls 5.5 dB in
a slow ramp starting where the far end switches to the data
constellation (-17.1 dB to -22.6 dB; the ATA doing something to a
signal with a real envelope, unexplained) but it does it on every call,
good and dead alike, and the gain follows it. What differs is the
carrier: during TRN and R2 every call's frequency estimate reads about
+0.1 Hz, and across the seam -- AR3 reading the far end's E and then its
B1 with a four-point slicer behind a gate that a dense constellation
passes one symbol in five, then AE and the first data blocks with the
dense-grid slicer -- the loop's integrator walks off by one to three
hertz on the calls that die. A decision-directed loop on 64 or 128
points cannot bring that back: with the constellation more than a
fraction of a cell off, its phase error against the nearest point is
noise, and `phErr` is an unweighted `atan2`, so the inner points -- at
a radius of 0.16 on the 128 grid, where 0.07 of noise is 24 degrees --
weigh as much as the outer ones. The integrator random-walks, the
constellation spins, and the call is dead from its first symbol at a
reported error that looks marginal. Which way the walk goes is the
call-to-call lottery; at 12000 it lands on the locked side often, at
14400 rarely, because the 128 grid has more inner points, finer cells
and one decision in five wrong even when locked.

**The intervention that settles it.** `modec replay --v32-freq 0.1`
sets the data pump's carrier frequency estimate to the start-up's own
reading at the first data block and holds it there (no integral term).
Same recordings:

| call | plain | `--v32-freq 0.1` |
| --- | --- | --- |
| 12000 `ber` 40 dB | dead, 50 % ones | **locked from +0.5 s**: 99-100 % ones, true 0.012-0.017 (18-19 dB), whole call |
| 14400 `235126` | dead | **locked from 18.6 s**: 82-92 % ones, true 0.010-0.013 (19-20 dB) for the next 40 s |
| 14400 `032546` | dead until 46 s | **locked from 16 s**: 93-100 % ones, true 0.0053-0.0074 (21-23 dB) |
| 14400 `-g08` | dead | 59-78 % ones, true 0.015-0.027 (16-18 dB) for 12 s, then lost |
| 12000 `-ec`, `-noec`, `042805`; 14400 `031609` | locked | unchanged |

Holding the handover's own value (`--v32-hold-freq`) does nothing for
the dead calls, because by the handover the value is already wrong;
it is the value the seam leaves behind. The tolerance is narrow: a
preset of 0.0 Hz still locks most of the 12000 call and a fifth of the
14400 one, 0.4 Hz locks almost nothing, 0.7 Hz and beyond nothing at
all -- a 0.4 Hz ramp under a 0.03 proportional gain is two degrees of
steady phase lag, and two degrees at the outer points of the 128 grid
is half the decision margin. The receiver has to be handed the
frequency to within a tenth of a hertz and then not allowed to lose it.
That the 14400 calls lock from 16 or 18 s rather than from CONNECT is
the phase half of the same weakness: with the frequency right, the
decision-directed phase loop on 128 points still needs seconds of luck
to find the constellation, and on `-g08` lost it again.

**Where the samples stand.** A least-squares 31-tap equaliser fitted
to the good 12000 call's own T/2 samples against the predicted symbols
(`--dump-syms`, a scratchpad numpy fit) leaves 0.0032-0.0034 -- 24.9 dB,
63 taps no better -- where the receiver read 0.0028-0.0029. Locked, the
receiver is *at* the linear floor of its input. The path therefore
supports 25 dB at the receiver's output on this bench, echo and all,
which is what 14400 needs with room to spare; the locked 14400 calls
read 19 to 26 dB. So nothing in the sections above about margin, echo
under the floor, or the loops on a dense grid costing a decibel here
and there was the cause. They are the next tier, and they are worth
one to three decibels between them; the cause was a receiver that was
not reading the constellation at all, and a metric that could not tell.

**What the seam taught on the way**, since `aidB1` was built on it.
Predicting B1 from E does not work against this far end, for three
reasons found one at a time. The CX93001 sends E with B14 set, a bit
Table 5 reserves and `decodeSeq` rightly ignores, so an E re-encoded
from the decoded rate sequence leaves the line at that bit and the
scrambler register predicted from it is never the far end's. The raw
differential decision the detectors read is wrong in about one bit in
sixteen to thirty-two on a real line (`detectE` allows for exactly
that), and one wrong bit in the twenty-three that make a register is a
register that is not the far end's; the equalised points read the four
states at 0.03 and their turns are clean, but they lag the raw symbols
by the equaliser's seven-symbol delay, so at the moment E is detected
its last equalised point has not been produced yet and B1 has not
arrived. And a quarter turn of a trellis sequence is not a trellis
sequence from state zero -- Y0 flips under 90 degrees and the encoder's
first Y0 is always 0 -- so of the four frames that fit E, only the
receiver's own predicts B1, and a check by quadrant, which is what the
"nine of nine" above was, cannot tell them apart: the wrong frames
agree by quadrant and put every point in the wrong subset. Even with
all three put right, the points received during AE did not descramble
to ones at any encoder state, quadrant or register offset. Whether the
far end's B1 is something other than scrambled ones from the register
E leaves, or the receiver at the seam is simply not reading it, is
open; the pump-state prediction sidesteps the question, and `aidB1`
as shipped cannot have been matching on a real call.

**What follows.** The fix is in the carrier loop and the seam, not in
the equaliser or the echo: carry the start-up's frequency estimate
into data mode and take the integrator away from the dense-grid
decisions, or make it very slow, until they are trustworthy; weight the
phase detector by the decision's radius or drive it from the trellis
decoder's delayed decisions rather than the immediate slicer; keep the
seam's four-point receiver from steering on the far end's B1 at all;
and give the byte gate and the retrain timer a metric that does not
saturate -- the ones share of the descrambled idle is one, the trellis
path metric another. Then the tiers above -- echo, timing jitter, the
2 dB the aided oracle measured -- become worth chasing, because a
locked 14400 receiver on this bench reads 19 to 23 dB against a need
of about 22.

Tools left behind, all off unless asked: `replay --truth`,
`--train-truth` (train the loops on the prediction: the aided oracle
on a recording), `--v32-evm-freeze E`, `--v32-no-acq`,
`--v32-hold-freq`, `--v32-freq HZ`, `--dump-syms FILE` (one row per
symbol: time, the two T/2 samples into the equaliser, the equalised
point, the decision, the predicted point), and the `loops` and `taps`
lines that `--line` now prints through the start-up as well as data
mode.

### The fix, and what it leaves (2026-09-12)

Four changes, each aimed at the mechanism above, measured on the eight
recorded calls and on the corpus.

**The seam stops steering the frequency.** From R3 onward the estimate
has had seconds of TRN and R2 on four points and is within a tenth of a
hertz; nothing the far end sends afterwards can improve it, and the
dense constellation it sends can destroy it. `v32SeamCfg` and the new
`v32SeamDataCfg` therefore run AR3, AE and OB1 with no integral term,
and with the loop gate applied from the first symbol rather than only
once the receiver has locked -- `qamRxUnlock`, which the handover calls
by design, used to stand the gate down at exactly the wrong moment.

**Data mode has no integral term at 12000 and 14400.** The same knee as
`narrowTiming`: above it a decision-directed integrator is a random walk
driven by decisions that are wrong one time in five, and what it walks
away from cannot be recovered. The proportional term cannot run away,
and a tenth of a hertz held by a gain of 0.03 is half a degree of lag.
Below the knee nothing changes.

**The phase detector is weighted by the decision's radius** -- the cross
product rather than `atan2` -- for trellis rates. An inner point of the
128 set sits at radius 0.16 where a readable line's noise is 24 degrees;
an outer one reads the same noise as 2 degrees. A/B on the eight calls:
better or equal on every one, and decisive on the marginal two (14400
`-g08` 15.0 dB against 8.6, 12000 `-noec` 19.2 against 17.5).

**The byte gate reads the far end's idle instead of the saturating
error.** A run of 256 descrambled ones cannot be had by a receiver out
of step with the far end's scrambler, and B1 alone supplies it. Either
that proof, seen within the last second, or the old error gate now
counts as trust -- so the rates whose decision error still means
something are untouched, and 14400 stops being refused by a gate it
cannot pass. The retrain timer is held off for the first three seconds
of a link, because the evidence it waits for is an idle the far end does
not owe us.

**What it does.** Every one of the eight recordings now locks, with the
default byte gate and no `--max-evm-v32`:

| call | before | after |
| --- | --- | --- |
| 12000 `ber` 40 dB | never locked | 16.3 dB, 4005 bytes |
| 14400 `235126` | never locked | 17.1 dB, 7307 bytes |
| 14400 `-g08` | never locked | 15.0 dB, 7412 bytes |
| 14400 `032546` | never locked | 14.9 dB, 3 of 8 lines |
| 14400 `031609` | 21.6 dB, 8 lines | 21.9 dB, 8 lines |
| 12000 `-ec` | 21.1 dB, 8 lines | 21.2 dB, 8 lines |
| 12000 `-noec` | 18.7 dB, 8 lines | 19.2 dB, 8 lines |
| 12000 `042805` | 20.2 dB | 20.4 dB |

**And what it leaves.** Fitted against the far end's own symbols, a
least-squares 31-tap equaliser on the same T/2 samples now reads within
about a decibel of what the receiver reads: 26.0 dB against a 25.3 dB
floor on the good 14400 call (the receiver beats a fixed fit because it
adapts), 19.9 against 20.7 on the long one, 17.1 against 19.2 on the
noisy 12000. So the receiver is at the linear floor of its input, and
the calls that still lose bytes lose them because that floor is 20 dB
where 14400 wants 22. That is the line, and it is where the echo work
and the two decibels the data-aided oracle measured belong.

Two things the corpus found, which is what it is for. The third-party
Banksia 14400 fixtures had pinned an *empty* decode with a note saying
the two modems idled. They did not: the far end sends V.42's detection
pattern -- 132 repetitions of "EC" on one side, 0x11 0x91 on the other
-- and then flag-delimited LAPM frames, and modec had been reading them
at a decision error of 0.0015 and refusing to pass them. The framer's
old arming rule wanted half a second of data mode to have elapsed
before it would believe a run of descrambled ones, the one run that far
end ever sent arrived a tenth of a second early, and nothing armed it
again for the rest of the call. Both fixtures now pin those bytes, which
is a far stronger assertion than emptiness. And the CX93001 9600
fixture moved by nine bytes of idle, the payload byte for byte
unchanged, because the framer opens about a block earlier.

### Roles reversed: modec dials (2026-09-12)

T1.1 wants both directions, and this section used to say the ATA would
not allow it: the HT802V2 ignored every INVITE and OPTIONS sent to 5060
and 5062, and the conclusion drawn was that it will not terminate a call
from a proxy it is not registered to. That was wrong. **Its SIP stack
listens on a random port** (22097 on this boot), says nothing at all on
5060, and answers OPTIONS at the right port with a 200 OK, unregistered.
No registrar is needed. Its own INVITE names the port in `Contact:`, so
`sweep.py` finds it by having the reference dial out while nothing holds
5060 and reading the INVITE off the socket; the answer is cached in
`recordings/bench/ata-sip.txt` and re-checked with OPTIONS each run, and
`ATA_SIP=host:port` overrides it. Setting "Use Random SIP Port" to No in
the ATA would make it 5060 for good. modec then dials
`ATD1001@192.168.30.105:22097` -- `dialUri` passes anything with an `@`
through -- and the reference picks up on `ATS0=1`:

    scripts/bench/sweep.py -o v22bis v32b-9600    # -o: modec places the call

**The first pass found three V.32 caller failures that answering never
could.** Bell 212A, V.22, V.22bis, V.32 9600 and V.32bis 7200 and 9600
carried the payload both ways at once; Bell 103 and V.21 were error free
both ways for as much of it as 16 s at 300 bit/s holds. 4800 never
connected; 12000 carried modec's data and not the reference's; 14400
carried neither. Every one of them came out of the calling start-up,
which until now had only ever run against modec itself and against
2600.network, and three separate defects were stacked in it.

**It answered ringback.** baresip plays North American ringback, 440 +
480 Hz, into the modem while the ATA rings, exactly as a PSTN line would.
It leaks into the 600 Hz tracker well over the threshold `OListen` used,
and its 40 Hz beat reads as a phase reversal every 12 to 27 ms. The
caller sent AA at 0.36 s -- two seconds before the answer tone -- took two
beats for the far end's two reversals, and measured a round trip of 40
ms. AC is steady until the answerer has heard AA, so `OListen` now wants
256 symbols of AC with no reversal in it; the replay goes to AA at 8.9 s,
on the real AC, and measures NT as about 0.9 s. The third-party Banksia
fixture had the same false start (AA at 3.70 s, inside the answer tone, a
60 ms "round trip") and its reference was minted from it; it is re-minted,
the V.42 exchange byte for byte unchanged and 88 bytes of unreadable tail
moved, at the same decision error (0.01968 before, 0.01966 after).

**It took R1 for R3.** The answering modem sends R1 until it hears our S,
so R1 goes on arriving for a round trip after S begins -- a second, on
this path, which is longer than our S and TRN together. `OR2` waited 128
symbols and accepted the first rate signal it saw: R1, three seconds
before R3. It then waited for an E anchored on the wrong sequence. `OR2`
now discards what it hears until NT plus 512 symbols after our S began;
the far end's own S, S-bar and TRN, at least 1552 symbols, still separate
that moment from its R3. Both fixes were needed: the gate is measured in
NT, and NT had been 40 ms.

**It read R3 and E through the line's ISI.** With both fixed, the 4800
caller reached `OB1` on time and still never saw E. The rate-signal bits
were taken from the change between *raw* symbols -- before the equaliser,
chosen so the S and AC detectors owe nothing to the carrier loop -- and
this line's neighbouring-symbol taps are 0.15 of the main one. R3 read
with 17% of its descrambled bits wrong (the descrambler triples each
error: the gaps between them cluster at 5, 18 and 23) while the equalised
points sat within fifteen degrees of their states, not one of 1680 in
doubt. E is sent once and tolerates one error in its seven fixed bits.
In `OR2`, `OB1` and the answerer's `AR3` -- after a TRN the receiver has
trained on -- the bits now come from the equalised points: R3 reads
without a single error in any 512-bit window, on the 4800 call and on a
9600 call that had been getting E through by luck.

The 4800 call is now a fixture, `cx93001-answering-v32-4800`: the build
before these fixes replays it to 0 bytes, this one to CONNECT at 19.78 s
and all eight of the reference's lines.

**After the fixes, both directions, every V.32 rate:**

| rate | modec answers | modec calls |
| --- | --- | --- |
| V.32 4800 | payload both ways | payload both ways (never connected before) |
| V.32 9600 | payload both ways | payload both ways |
| V.32bis 7200 | payload both ways | payload both ways |
| V.32bis 9600 | payload both ways | payload both ways |
| V.32bis 12000 | payload both ways | payload both ways (reference's data lost before) |
| V.32bis 14400 | payload both ways | the reference refuses the rate |

Against the recordings of every V.32 call modec placed over voip.ms in
September -- nine that connected at 4800 to 14400, one that failed "no E
from the answering modem", one timeout -- the fixed start-up and the one
before it replay byte for byte the same: same rate at the same instant,
same retrains, same bytes, AA a tenth of a second later. Those calls ran
V.8, which starts V.32 after the answer tone and so never met ringback,
and their far ends' R3s read the same either way. The two failures stay
failures; they are something else.

The 14400 caller failure is not a detection fault. Twice since the fixes
the CX93001, pinned to 14400 with automode off, answered our R2 with an
R3 of `0000 0001 1001 0001` -- the trellis bit and no rate -- and modec
read it correctly and cleared. The answering modem chooses R3 from what
its receiver made of our TRN, so the CX93001 judged the line in the
modec-to-reference direction not good enough for 14400 when modec is the
caller, and good enough when modec answers. Our S lasts the same 1.05 s
either way, as does the CX93001's own. Before the fixes one caller call
did train at 14400, and the reference received garbage from it. So the
open question is modec's *calling* transmitter at 14400 (GPC scrambler,
caller TRN) against its answering one, which is proven. That is where to
look next, not at the path.

One more thing the reversed direction shows: the ATA re-INVITEs once,
after the far end's answer tone, on nine of the first eleven calls modec
placed (not on Bell 103 or Bell 212A). The calls
carry data regardless, but it is a media renegotiation mid-call and
"Re-INVITE After Fax Tone Detected" is the setting to check.

## Tier 1 — the questions that are open now

### T1.1 Does modec's 14400 receiver carry a real modem's data?

The headline. In loopback, 14400 connects every time and delivers the
payload none of the time, with a slicer error of 0.003 — clean symbols,
wrong bytes — while the same pump offline reads 14400 perfectly. Both
ends of that loopback are modec, so the two of them can be wrong
together and still agree. A reference implementation is the only thing
that breaks the symmetry.

```
reference:  AT&F  AT\N0  AT+MS=V32B,0,14400,14400  ATS0=1
modec:      cabal run modec -- modem --answer --sip 127.0.0.1:4444 \
              --audio-sip-loop modec --hayes --mode v32bis --v32-rate 14400 --listen 2323
```

Dial in from the reference, then send a known pattern from the reference
side and compare byte for byte. Repeat with the roles reversed
(`modec dial`, reference answering).

**Records:** `recordings/<stamp>-*.wav` and `-tx.wav` both ways.

**Reads as:**
- reference→modec clean, modec→reference corrupt → modec's *transmitter*
- reference→modec corrupt, modec→reference clean → modec's *receiver*
- both clean → the fault is in modec talking to itself (harness or a
  transmitter/receiver pair that agree on something wrong)
- both corrupt → modec's 14400 is broken end to end

The fourth outcome is the least likely and the most useful; the third
would be the most surprising and would send us straight back to
`Modec.Loopback`.

### T1.2 The same at 12000, 9600 and 7200

```
AT+MS=V32B,0,12000,12000        then 9600, 7200
AT+MS=V32,0,9600,9600           V.32 proper, trellis
```

12000 fails intermittently in loopback (1–2 of 3 seeds) and 9600
never fails. Finding the same boundary on hardware confirms it is real;
finding a different one says the simulator is misleading us.

### T1.3 The echo cliff — topology B only

The finding to test: modec's V.32 start-up gives up at `no reversal in
AC` once the near-end echo is roughly **2 dB above** the far end's
arriving signal, because the AC reversal detector normalises a
narrowband correlation by wideband power ([QAM.hs:515](../src/Modec/QAM.hs#L515)).
The echo canceller cannot help — it is not aimed until the conditioning
phase, about 1.5 s later.

The ATA path will not show this; its echo is too small. The two-wire
loop will, and you can *tune* it: the feed resistors set how badly the
hybrids are balanced, and adding series resistance in the pair raises
echo relative to the far signal.

1. Establish V.32 9600 over the loop. Note whether it comes up at all.
2. Attenuate the **far** end only (a resistive pad on the reference
   modem's side of the transformer, not in the shared DC loop) in 3 dB
   steps.
3. Record the step at which modec stops reaching CONNECT, and what
   `modec:` prints when it fails.

**Pass:** modec reaches CONNECT wherever the reference modem does.
**Expected failure:** modec gives up several steps earlier, with
`no reversal in AC`, while the reference is still connecting. That
measurement is the justification for fixing the detector.

### T1.4 Is the loopback's echo model right?

`test/Harness.hs`'s `modemDuplexEcho` adds the echo *before* the line
attenuation, so its taps are attenuated along with the far signal;
`Modec.Loopback`'s `lcEcho` adds it after, on the grounds that a
reflection off our own hybrid never crosses the line. They disagree by
the whole line loss — 20 dB — and they cannot both be right.

Measure the real thing: on topology B, transmit a known tone from modec
with the reference modem on-hook, and record what comes back. The ratio
of returned to transmitted, against the far end's arriving level when
the reference is off-hook and transmitting, is the number both harnesses
are trying to model. `modec modem --probe` holds a V.32 carrier and
measures the echo path for exactly this.

## Tier 2 — ground truth for the corpus

These are cheap, and they are the lasting value: fixtures outlive the
bench session.

### T2.1 A `role: answer` fixture for every mode

For each of Bell 103, V.21, Bell 212A, V.22, V.22bis, V.32, V.32bis:

```
reference:  AT&F  AT\N0  AT+MS=<carrier>,0,<rate>,<rate>   then dial modec
modec:      cabal run modec -- modem --answer --sip 127.0.0.1:4444 \
              --audio-sip-loop modec --hayes --mode <mode>
```

Then mint each one:

```
cabal run modec -- replay --answer recordings/<stamp>-<caller>.wav --mint <name>
```

Note `--answer`: the whole point is that these are the first fixtures
where modec was the answering side.

### T2.2 Bell 212A, finally

`bbslist.txt` records that Bell 212A was attempted against twelve boards
and connected with none — every one answers ANSam then V.22. The
reference modem will offer the real Bell 212A answer sequence:

```
ATB1                    Bell answer sequence at 1200
AT+MS=B212,0,1200,1200
```

Both directions. This is the only way to exercise `AProbe Bell103`'s
shared 2225 Hz rung and `bell212Trigger` against a genuine Bell answerer
rather than against modec's own idea of one.

### T2.3 V.8 present and absent

```
AT+MS=V32B,1,...        automode on -- the modem picks, and uses V.8
AT+A8E=...              V.8 controls, if the firmware exposes them
```

versus a pinned `AT+MS=V32B,0,...`. modec's V.32 answering rung (Annex
A.2.2, committed as `a9f2e27`) has **never been exercised against
anything but itself**. A reference modem that dials without V.8 is the
first real test of it.

## Tier 3 — conformance probes

### T3.1 Retrain (V.32 §5.5)

Force one from the reference side mid-call — on the two-wire loop, briefly
short the pair or drop the level hard for ~500 ms, then restore.

**Pass:** modec logs `retraining the link`, the call survives, and data
resumes. `modemDuplexDisturb` covers this in simulation; hardware is the
check that a real modem's retrain sequence is what modec expects.

### T3.2 Rate renegotiation (V.32bis)

Degrade the loop slowly with the reference in automode
(`AT+MS=V32B,1,4800,14400`).

**Pass:** the pair steps down and modec reports `now running at N bit/s`.
This is the least-tested path in the codebase.

### T3.3 Cleardown (V.32 §5.6)

Hang up from the reference with `ATH` mid-transfer and watch what modec
makes of the cleardown sequence versus a bare carrier loss.

### T3.4 V.42 detection against a modem that speaks LAPM

modec has MNP but **no LAPM**. A reference modem with `AT\N3` will send
V.42 ODP and expect ADP.

**Pass:** modec's MNP link requests are not mistaken for V.42, and the
pair settles on MNP or on no error correction — not on a hung
negotiation. `mnpProbeGap` exists because of exactly this timing.

### T3.5 Guard tones

`AT+MS=V22B,...` with the reference set for 1800 Hz guard tone (a
country-setting or an S-register, firmware dependent). modec does not
transmit one by default; check it *tolerates* one on receive.

## Tier 4 — needs more hardware

### T4.1 Two modems, as a reference pair

With a second USB modem on the other ATA port, capture a
reference-to-reference handshake for every mode. That is ground truth
with modec removed from the loop entirely — the thing to diff against
when modec and one reference disagree and neither is obviously wrong.

### T4.2 Voice mode as a transducer

**Answered: yes, on a CX93001.** `AT+FCLASS=8`, then `+VSM`, `+VSD=0,0`,
`+VIT=0`, `+VLS=1` and `+VTR` gives `CONNECT` and a duplex stream at
exactly 8000 bytes/s, whose spectrum off an idle FXS port is dial tone
(348.6 and 438.5 Hz at equal level, everything else 37 dB down).

Do **not** gate this on `AT+VTR=?`. It returns ERROR, and so do
`AT+VRX=?` and `AT+VTX=?`, which are mandatory Class 8 commands: this
firmware simply has no test form for action commands. Gating on it
abandons a working transducer on a false negative. Issue `+VTR` itself,
off hook, and see whether samples arrive.

`--audio-serial` therefore works, in `ulaw`; see
[asterisk.md](asterisk.md) for why not in `pcm14`. `+VRX` remains a
clean recording of the far end straight off the hybrid — corpus fixtures
with no sound card and no transformer.

modec's native rate is 8 kHz (`app/Modem.hs:112`) and `Modec.G711`
already has the µ-law codec, so a voice-mode backend needs no
resampling and no new DSP — only DLE unshielding.

## Recording discipline

Every call should leave a WAV, a `-tx.wav` and a log; `modec answer` and
`modec dial` do this by default under `recordings/`. Before each session:

```
AT&F ... ATI3           paste the firmware string into the session notes
```

and record, for every call, the exact `+MS=` string on the reference
side. A fixture whose modulation is inferred rather than commanded is
worth much less than one where both ends were pinned.

The single most valuable artefact from a first session is **T1.1 in both
directions with both recordings kept**, because it decides where the
14400 fault lives, and that question is currently blocked on having no
second implementation to ask.
