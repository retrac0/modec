# modec

A software audio modem written from scratch in Haskell. Audio in and out
via PipeWire (later SIP/RTP with G.711), bytes in and out as telnet
streams. Targets, in order: Bell 103, V.21, V.22, V.22bis, V.8bis link
establishment.

See [SURVEY.md](SURVEY.md) for the survey of existing work and the design
plan, [docs/line-interface.md](docs/line-interface.md) for hooking a real
modem to a sound card, [docs/negotiation.md](docs/negotiation.md) for how modes are detected and
negotiated, [docs/sip-options.md](docs/sip-options.md) for the VoIP/SIP plan,
[docs/voipms.md](docs/voipms.md) for dialling out through voip.ms, and [docs/recordings/](docs/recordings/) for recorded handshakes
with an annotated timeline.

## Status

- Bell 103 and V.21 asynchronous FSK modulator and demodulator,
- Receiver: band-pass prefilter, O(n) prefix-sum tone correlators,
  adaptive slicer, UART-style framer with sub-sample start-edge location,
  one timing correction per bit boundary, integrate-and-dump decisions and
  a start-bit depth check over the middle 60 % of the bit (rejects the
  switch-on transients of a strong adjacent channel).
  Error free in the bench down to 6 dB SNR, ±3 % clock offset, ±30 Hz
  carrier offset, jitter, slips and +20 dB adjacent channel.
- Transmitter: continuous-phase FSK with transmit band limiting.
- Channel simulator (`Modec.Channel`): telephone band-pass, AWGN by SNR,
  clock offset, sinusoidal / random-walk / slip jitter, frequency offset,
  dropouts, echo, clipping, hum, DC, level; deterministic from a seed.
- Tone bank and detection (`Modec.Detect`): per-20 ms tone amplitudes,
  dominant-tone runs, offline FSK standard/channel identification.
- Call establishment (`Modec.Handshake`): V.25 answer sequence with
  Bell 103, V.21, Bell 212A, V.22 and V.22bis on both sides, verified by
  duplex simulations in the test suite. `--modes` chooses which of them
  the modem will negotiate, in order of preference; see
  [docs/negotiation.md](docs/negotiation.md) for the decision tree and a
  walkthrough of what happens when each kind of modem calls in.
- Live modem (`Modec.Modem`, `modec modem`): audio in and out through
  PipeWire (`pw-cat`) or raw 8 kHz s16le pipes, data through a telnet
  server or client (BINARY and SUPPRESS-GO-AHEAD negotiated, IAC
  escaped) or stdio. Automode on both ends. `scripts/smoke-loopback.sh`
  cross-connects two instances through FIFOs and pushes text both ways.
- Bell 212A: the V.22 data pump and handshake timings announced with the
  2225 Hz Bell answer tone instead of unscrambled binary 1, no guard tone
  and no 2400 bit/s rate, exactly the substitution V.22 §6.3.1.1 notes. It
  shares the Bell 103 probe tone, and the caller's reply tells the two
  apart.
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
  with a V.22-only peer. `--modes v22` disables 2400 on our side.
- V.8bis capabilities exchange (`Modec.V8bis`, `Modec.Hdlc`): after the
  billing delay the answerer sends CRe (dual tone 1375 + 2002 Hz, then
  400 Hz), a V.8bis caller replies with ESr and a CL message over V.21
  (HDLC frames with the ISO 3309 FCS, synchronous 300 bit/s), the answerer
  selects the best common mode with MS, and the V.25 start-up follows with
  the roles reversed as §9.9.3 requires (the MS receiver becomes the
  answering modem). Peers without V.8bis get the classic start-up after
  3 s. `--no-v8bis` skips it.
- Hayes AT command mode (`Modec.Hayes`, `--hayes`): ATD (digits dialled as
  DTMF, then originate), ATA, ATH, ATO, ATZ, AT&F, ATE/V/Q, ATI, ATS0
  (auto-answer on a sustained calling signal), "+++" with guard times;
  result codes OK, CONNECT 300/1200/2400, RING, NO CARRIER, ERROR.
  `scripts/smoke-hayes.sh` drives two instances through a full
  dial/answer/data/escape/hang-up cycle over telnet.
- SIP calls through baresip (`Modec.Baresip`, `--sip HOST:PORT`): modec
  drives baresip's `ctrl_tcp` module (netstring-framed JSON) and maps
  Hayes commands to it: ATD dials `sip:NUMBER@--sip-domain` (or a full
  URI), ATA accepts, ATH hangs up, an incoming call rings the DTE, and the
  modem starts in the right role when baresip reports the call established.
  Audio reaches the softphone through a PipeWire loopback pair created by
  `--audio-sip-loop` (see `docs/baresip/` for the baresip configuration).
  `scripts/smoke-sip.sh` runs the whole control path against a fake
  baresip pair, and [docs/voipms.md](docs/voipms.md) walks through a real
  provider.
- Session recording: `--record-rx` and `--record-tx` write everything
  heard and sent to WAV files, which `modec probe` and `modec detect` read
  back. The length fields are refreshed twice a second, so a recording is
  usable even if the process is killed mid-call.
- WAV reader (PCM 8/16/24/32, float 32) and 16-bit mono writer.

Known limits: V.21 tolerates an adjacent channel up to about +25 dB (its
channels are only 470 Hz apart); Bell 103 to beyond +30 dB. Dropouts lose
the characters they hit. Clock offsets beyond ±3 % fail.

Known 2400 bit/s limits: re-acquisition after jitter-buffer slips is slow
(each slip costs tens of bytes) and heavy sinusoidal jitter breaks the
coherent path.

Not yet: SIP/RTP, ring detection (a sound card carries no ringing; ATS0
answers on sustained line energy instead), native PipeWire node (pw-cat
child processes are used instead), V.22bis guard tone by default, V.8bis
MR/ESi-initiated transactions and the V.8 start-up variants.

## Usage

```
cabal build
cabal test
cabal run modec -- decode test/fixtures/bell103_ans_8k.wav       # auto channel
cabal run modec -- decode --originate --v21 file.wav
printf 'hello\r\n' | cabal run modec -- encode --answer -o out.wav
cabal run modec -- devices                                        # PipeWire audio devices
cabal run modec -- probe recording.wav                            # tone energies
cabal run modec -- detect recording.wav                           # which standard, tone runs
cabal run modec-bench -- --channel answer --bytes 400             # impairment sweep (also v21, v22low/high, v22bislow/high)

# live modem: answer calls arriving on the default PipeWire source, serve telnet on 2323
cabal run modec -- modem --answer --audio-pipewire --listen 2323
# call out: audio through PipeWire, bytes to a telnet host
cabal run modec -- modem --originate --audio-pipewire --connect bbs.example.org --port 23
# pick the PipeWire node explicitly, force Bell 103, skip the handshake
cabal run modec -- modem --answer --audio-pipewire --pw-in usb --standard bell103 --no-handshake --listen 2323
# only the North American modes, best first
cabal run modec -- modem --hayes --audio-pipewire --modes bell212a,bell103 --listen 2323
# loop two instances through FIFOs with no sound card
scripts/smoke-loopback.sh
# Hayes mode: a terminal program talks AT commands over telnet; ATDT dials with DTMF
cabal run modec -- modem --hayes --audio-pipewire --listen 2323
scripts/smoke-hayes.sh

# SIP: install baresip, copy docs/baresip to ~/.baresip and edit accounts, run baresip, then
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain sip.provider.example --audio-sip-loop modec --listen 2323
# and from a terminal program: ATDT<number> dials the BBS, ATA answers an incoming SIP call
scripts/smoke-sip.sh
```

With `--audio-pipewire` the PipeWire defaults are used. `modec devices`
lists what is available, and `--pw-in` / `--pw-out` select an input and an
output independently by node id, node name, or any unambiguous part of
either (`--pw-in usb --pw-out analog`); `--pw-target` sets both at once.
An unknown or ambiguous name is refused with a listing rather than
guessed. On a machine with no capture device modec records the playback
monitor instead and says so, which lets the modem hear its own tones;
`--pw-monitor` forces that mode. If the capture stream stops (device
unplugged, pw-cat killed) the modem reports NO CARRIER, restarts the
audio and carries on, giving up after three attempts. To play with it
live from a terminal:

```
cabal run modec -- modem --hayes --audio-pipewire --pw-monitor --data-stdio
ATE0          # the terminal already echoes what you type
ATA           # hear the V.8bis CRe and the 2100 Hz answer tone from the speakers
ATH
ATDT5551234   # hear DTMF, then the modem waits for an answer tone
``` Telnet clients see the negotiation immediately;
bytes flow once the log on stderr says `CONNECT`. `NO CARRIER` or a failed handshake ends the
process.

## Test fixtures

`test/fixtures/*.wav` were generated with minimodem (`minimodem --tx -f
out.wav -R <rate> [-M mark -S space] 300 < text`); the matching `.txt`
holds the payload. `bell103_ans_8k_noisy.wav` has sox white noise mixed in.
