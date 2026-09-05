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
- Call establishment (`Modec.Handshake`): V.25 answer sequence, Bell 103
  and V.21 originate/answer state machines with automode on both ends,
  verified by a duplex simulation in the test suite.
- Live modem (`Modec.Modem`, `modec modem`): audio in and out through
  PipeWire (`pw-cat`) or raw 8 kHz s16le pipes, data through a telnet
  server or client (BINARY and SUPPRESS-GO-AHEAD negotiated, IAC
  escaped) or stdio. Automode on both ends. `scripts/smoke-loopback.sh`
  cross-connects two instances through FIFOs and pushes text both ways.
- V.22 data pump (`Modec.V22`): 1200 bit/s, 600 Bd differential 4-PSK on
  1200/2400 Hz, RRC 75 % shaping, 1 + x^-14 + x^-17 scrambler, Gardner
  timing recovery, handshake signal detectors (unscrambled ones, S1,
  scrambled ones). Error free through the channel simulator down to 8 dB
  SNR, ±7 Hz carrier offset, ±1 % clock offset and jitter. Validated
  against spandsp's V.22bis test program at 1200 bit/s: its handshake
  signals are recognised in order and its BERT data decodes with zero
  PRBS-11 recurrence failures. Not yet wired into the handshake and the
  live modem.
- WAV reader (PCM 8/16/24/32, float 32) and 16-bit mono writer.

Known limits: V.21 tolerates an adjacent channel up to about +25 dB (its
channels are only 470 Hz apart); Bell 103 to beyond +30 dB. Dropouts lose
the characters they hit. Clock offsets beyond ±3 % fail.

Not yet: V.22 call establishment and data mode in the live modem, V.22bis
(16-QAM, needs carrier recovery and an equaliser), V.8bis, SIP/RTP, Hayes AT
command layer, native PipeWire node (pw-cat child processes are used instead).

## Usage

```
cabal build
cabal test
cabal run modec -- decode test/fixtures/bell103_ans_8k.wav       # auto channel
cabal run modec -- decode --originate --v21 file.wav
printf 'hello\r\n' | cabal run modec -- encode --answer -o out.wav
cabal run modec -- probe recording.wav                            # tone energies
cabal run modec -- detect recording.wav                           # which standard, tone runs
cabal run modec-bench -- --channel answer --bytes 400             # impairment sweep

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
