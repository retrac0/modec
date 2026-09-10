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

### Roles reversed: blocked at the ATA

T1.1 wants both directions. With modec dialling, the HT802V2 **ignores
every SIP message from this host** -- INVITE, OPTIONS, on 5060 and
5062, not even a 100 Trying -- while cheerfully sending REGISTERs that
baresip cannot honour. It originates with `Outgoing Call without
Registration` but will not terminate a call from a proxy it is not
registered to. Two ways out: a setting in the ATA that accepts inbound
from the proxy while unregistered, or a real registrar. `opensips` is
in `extra`; Asterisk is AUR and also buys the progress tones and
per-leg recording in [asterisk/](asterisk/).

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
