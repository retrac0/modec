# modec

A software audio modem written from scratch in Haskell. Audio in and out
via PipeWire (later SIP/RTP with G.711), bytes in and out as telnet
streams. Targets, in order: Bell 103, V.21, V.22, V.22bis, V.8bis link
establishment.

See [SURVEY.md](SURVEY.md) for the survey of existing work and the design
plan, and [docs/line-interface.md](docs/line-interface.md) for hooking a
real modem to a sound card.

## Status

- Bell 103 and V.21 asynchronous FSK modulator and demodulator, both
  channels, any sample rate, streaming (chunk-invariant) with an explicit
  state machine per stage (`Modec.Stream`). Cross-validated both ways
  against minimodem 0.24.
- Receiver: band-pass prefilter, O(n) prefix-sum tone correlators,
  adaptive slicer, UART-style framer with sub-sample start-edge location,
  one timing correction per bit boundary and integrate-and-dump decisions.
  Error free in the bench down to 6 dB SNR, ±3 % clock offset, ±30 Hz
  carrier offset, jitter, slips and +20 dB adjacent channel.
- Transmitter: continuous-phase FSK with transmit band limiting.
- Channel simulator (`Modec.Channel`): telephone band-pass, AWGN by SNR,
  clock offset, sinusoidal / random-walk / slip jitter, frequency offset,
  dropouts, echo, clipping, hum, DC, level; deterministic from a seed.
- Tone bank and detection (`Modec.Detect`): per-20 ms tone amplitudes,
  dominant-tone runs, offline FSK standard/channel identification.
- Call establishment (`Modec.Handshake`): V.25 answer sequence, Bell 103,
  V.21 and V.22 originate/answer state machines with automode on both
  ends, verified by duplex simulations in the test suite.
- Live modem (`Modec.Modem`, `modec modem`): audio in and out through
  PipeWire (`pw-cat`) or raw 8 kHz s16le pipes, data through a telnet
  server or client (BINARY and SUPPRESS-GO-AHEAD negotiated, IAC
  escaped) or stdio. Automode on both ends. `scripts/smoke-loopback.sh`
  cross-connects two instances through FIFOs and pushes text both ways.
- V.22 / V.22bis data pump (`Modec.V22`): 600 Bd on 1200/2400 Hz, RRC
  75 % shaping, 1 + x^-14 + x^-17 scrambler, Gardner timing recovery.
  1200 bit/s uses differential 4-PSK decisions; 2400 bit/s adds a coherent
  path: AGC, decision-directed carrier phase/frequency loop (frequency
  fed forward from the differential detector during training), a 15-tap
  T/2 LMS equaliser, and 16-way decisions on the Figure 2/V.22bis
  constellation in a quadrant-rotated frame. Handshake signal detectors
  for unscrambled ones, S1 and scrambled ones at either rate. Error free
  through the simulator: 1200 bit/s down to 8 dB SNR, 2400 bit/s down to
  12 dB, ±7 Hz carrier offset, ±0.5 % clock offset, 3 ms delay distortion,
  echo, +30 dB adjacent channel. Validated against spandsp's V.22bis test
  program at both rates: handshake signals recognised in order, BERT data
  decodes with zero PRBS-11 recurrence failures.
- V.22/V.22bis call establishment (§6.3, including the S1 exchange, the
  600/450 ms rate switch and the 32-ones completion) in the handshake and
  the live modem. Automode probes V.22 first, then V.21, then Bell 103,
  accepts a Bell 103 caller at any point, and falls back to 1200 bit/s
  with a V.22-only peer. `--max-1200` disables 2400 on our side.
- V.8bis capabilities exchange (`Modec.V8bis`, `Modec.Hdlc`): after the
  billing delay the answerer sends CRe (dual tone 1375 + 2002 Hz, then
  400 Hz), a V.8bis caller replies with ESr and a CL message over V.21
  (HDLC frames with the ISO 3309 FCS, synchronous 300 bit/s), the answerer
  selects the best common mode with MS, and the V.25 start-up follows with
  the roles reversed as §9.9.3 requires (the MS receiver becomes the
  answering modem). Peers without V.8bis get the classic start-up after
  3 s. `--no-v8bis` skips it.
- WAV reader (PCM 8/16/24/32, float 32) and 16-bit mono writer.

Known limits: V.21 tolerates an adjacent channel up to about +25 dB (its
channels are only 470 Hz apart); Bell 103 to beyond +30 dB. Dropouts lose
the characters they hit. Clock offsets beyond ±3 % fail.

Known 2400 bit/s limits: re-acquisition after jitter-buffer slips is slow
(each slip costs tens of bytes) and heavy sinusoidal jitter breaks the
coherent path.

Not yet: SIP/RTP, Hayes AT command layer, native PipeWire node (pw-cat
child processes are used instead), V.22bis guard tone by default, V.8bis
MR/ESi-initiated transactions and the V.8 start-up variants.

## Usage

```
cabal build
cabal test
cabal run modec -- decode test/fixtures/bell103_ans_8k.wav       # auto channel
cabal run modec -- decode --originate --v21 file.wav
printf 'hello\r\n' | cabal run modec -- encode --answer -o out.wav
cabal run modec -- probe recording.wav                            # tone energies
cabal run modec -- detect recording.wav                           # which standard, tone runs
cabal run modec-bench -- --channel answer --bytes 400             # impairment sweep (also v21, v22low/high, v22bislow/high)

# live modem: answer calls arriving on the default PipeWire source, serve telnet on 2323
cabal run modec -- modem --answer --audio-pipewire --listen 2323
# call out: audio through PipeWire, bytes to a telnet host
cabal run modec -- modem --originate --audio-pipewire --connect bbs.example.org --port 23
# pick the PipeWire node explicitly, force Bell 103, skip the handshake
cabal run modec -- modem --answer --audio-pipewire --pw-target alsa_input.usb-... --standard bell103 --no-handshake --listen 2323
# loop two instances through FIFOs with no sound card
scripts/smoke-loopback.sh
```

With `--audio-pipewire` the default PipeWire source and sink are used;
if the default source is a monitor (no capture device) pw-cat reports
"no target node available", so name the node with `--pw-target` (find it
with `pw-cli ls Node`). Telnet clients see the negotiation immediately;
bytes flow once the log on stderr says `CONNECT`. `NO CARRIER` or a failed handshake ends the
process.

## Test fixtures

`test/fixtures/*.wav` were generated with minimodem (`minimodem --tx -f
out.wav -R <rate> [-M mark -S space] 300 < text`); the matching `.txt`
holds the payload. `bell103_ans_8k_noisy.wav` has sox white noise mixed in.
