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
the reference modem's error correction, not modec's modulation.

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
modec:      cabal run modec -- answer --mode v32bis --v32-rate 14400 --no-mnp --listen 2323
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
modec:      cabal run modec -- answer --mode <mode> --no-mnp
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

```
AT+FCLASS=8
AT+VTR                  full duplex?  ERROR here ends this line of work
AT+VSM=?                sample formats
```

If `+VTR` errors, the modem is half-duplex in voice mode and cannot
carry V.22 or V.32 for modec. Even then `+VRX` is a clean recording of
the far end straight off the hybrid — corpus fixtures with no sound card
and no transformer.

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
