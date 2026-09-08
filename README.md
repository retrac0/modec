# modec

A software audio modem written from scratch in Haskell. Audio in and out
via PipeWire, or through a SIP softphone; bytes in and out as a telnet
stream, on stdio, or to a terminal. It dials, reads what the network
answers with, negotiates a modulation with the far end, and carries data
over it, with error correction if the far end has any.

- [SURVEY.md](SURVEY.md) -- existing work, and the design plan
- [docs/negotiation.md](docs/negotiation.md) -- how modes are detected and negotiated
- [docs/mnp.md](docs/mnp.md), [docs/mnp-bench.md](docs/mnp-bench.md) -- error correction, and what is confirmed against real hardware
- [docs/line-interface.md](docs/line-interface.md) -- hooking a real modem to a sound card
- [docs/sip-options.md](docs/sip-options.md), [docs/voipms.md](docs/voipms.md) -- why the SIP path is built this way, and dialling out through voip.ms
- [docs/robustness.md](docs/robustness.md) -- what recorded calls survive when the simulator degrades them
- [docs/recordings/](docs/recordings/) -- recorded handshakes, with an annotated timeline

## Pre-alpha

This is pre-alpha software and the version number is honest. Nothing is
stable: interfaces, subcommands and option names change from commit to
commit, and there is no release, no packaging and no compatibility
promise. What has been measured is marked below as measured -- through
the channel simulator, against minimodem and spandsp, or over recorded
calls -- and the rest has met nothing but itself. Several modulations
here have never carried a byte over a real telephone line, and one of
them cannot yet be put on a call at all. Expect to read the source.

## Modulations

| Standard | Rate | Line signal | On a call |
| --- | --- | --- | --- |
| Bell 103 | 300 bit/s duplex | FSK, 1070/1270 Hz calling, 2025/2225 Hz answering | yes |
| V.21 | 300 bit/s duplex | FSK, 980/1180 Hz (L), 1650/1850 Hz (H) | yes |
| V.23 | 1200/75 bit/s asymmetric | FSK, 1300/2100 Hz forward, 390/450 Hz backward | yes |
| Bell 212A | 1200 bit/s duplex | 600 Bd differential 4-PSK, 1200/2400 Hz, Bell answer tone | yes |
| V.22 | 1200 bit/s duplex | 600 Bd differential 4-PSK, 1200/2400 Hz | yes |
| V.22bis | 2400 bit/s duplex | 600 Bd 16-QAM on the same carriers | yes |
| V.18 Annex A (TTY/TDD) | 45.45 or 50 baud half duplex | FSK, 1400/1800 Hz, 5-bit Baudot text | offline only |
| V.32 | 4800 and 9600 bit/s, plain and trellis coded | 2400 Bd QAM on 1800 Hz | bench only |

The first six are what `--mode` chooses between and what automode
negotiates. The text telephone is `modec encode`/`decode` only: it
carries characters rather than bytes, so it has no place in a byte pipe.
V.32 has a coding layer, a start-up, a data pump and an echo canceller.
The canceller reaches about 18 dB of return loss on a call, but that is
not yet what decides whether a call through an echo carries data: see
the limits.

## What works

Measured through the channel simulator (`Modec.Channel`, all of it
deterministic from a seed):

- **the line** -- telephone band-pass, AWGN by SNR, level, DC, group
  delay, clock and carrier offset, hum, clipping
- **the loop and the hybrid** -- soft clip and a polynomial nonlinearity,
  so harmonics and intermodulation; echo as one tap, as several at
  fractional delays, or round a feedback loop, which is how a circuit
  near its singing margin rings
- **the carrier system** -- carrier wobble, and phase jitter in degrees
  peak to peak at power-line rates
- **the medium** -- wow, flutter and scrape, in percent speed deviation
- **the digital span** -- G.711 µ-law and A-law, bit errors in the PCM
  codes, stuck codes, bursty frame loss on a Gilbert-Elliott chain with
  silence, hold-last or repeat-frame concealment, and frame slips
- **transients** -- impulse noise arriving Poisson and ringing, gain
  hits and dropouts

Named profiles bundle these into paths that exist: `voip` (the one every
recording here came through), `long-loop`, `handset`, `tape`, `noisy`.
What each impairment does is checked against its own closed form -- the
harmonics against the polynomial's coefficients, companding against the
level sweep, the ring-down against the loop gain -- in `cabal test
--test-options='-p "what the channel does"'`. Error free means no byte
in the payload wrong, not a bit error rate.

| Mode | Error free through the simulator | Cross-checked against |
| --- | --- | --- |
| Bell 103, V.21, V.23 | 6 dB SNR, ±3 % clock, ±30 Hz carrier, jitter, slips, +20 dB adjacent channel | minimodem fixtures (Bell 103, V.21) |
| V.18 TTY, 45.45 and 50 baud | 4 dB SNR, text exact | minimodem 0.24, both directions, both rates |
| Bell 212A, V.22 | 8 dB SNR | spandsp's V.22bis test program |
| V.22bis | 12 dB SNR, ±7 Hz carrier, ±0.5 % clock, 3 ms delay distortion, echo, +30 dB adjacent channel | spandsp, BERT with no PRBS-11 failure |
| V.32 4800 | 12 dB SNR, and everything else the simulator offers, echo included | the Recommendation's own test vectors, on the coding layer |
| V.32 9600 | 18 dB SNR, 16 dB trellis coded, ±7 Hz carrier, ±0.5 % clock | ditto |
| MNP 2, 3 and 4 | every byte delivered where the bare link damages 3 % of them; links up at 4 dB | — |
| DTMF | digits to 3 dB SNR | Q.24's own accept and reject limits |
| Call progress | 3 dB SNR, and 30 dB below full scale | 221 recorded calls: 36 ringings, 159 answer tones, no false busy |

Twelve recorded calls are also in the suite, replayed through the whole
modem and checked against what the far end really sent: see
[test/fixtures/live/](test/fixtures/live/). Five of them are the same
access server answering at 300, 1200, 1200/75, 2400 and 9600 bit/s with
the same fixed banner, which is the closest thing to an oracle a dial-up
line offers. `cabal test` never places a call and never opens a device,
and the library it links carries no dependency that could.

## Protocols and signal path

- **FSK** (`Modec.FSK`): continuous-phase transmitter with band
  limiting; receiver of band-pass prefilter, O(n) prefix-sum tone
  correlators, adaptive slicer and a UART framer that locates the start
  edge to a fraction of a sample. `fskBurstDeframer` is its sibling for
  the carrierless modes, where a burst can open with the start bit and
  there is no idle mark to hunt in.
- **V.22 pump** (`Modec.V22`): 600 Bd, RRC 75 % shaping, the
  1 + x^-14 + x^-17 scrambler, Gardner timing recovery; differential
  4-PSK at 1200 bit/s, and at 2400 a coherent path of AGC,
  decision-directed carrier loop and a 15-tap T/2 LMS equaliser.
- **V.32** (`Modec.V32`, `Modec.V32Pump`) on `Modec.QAM`, which is the
  V.22 receiver with baud, carrier, shaping, tap count, loop gains and
  constellation lifted out into arguments. The coding layer is free of
  any sample rate, so its tables can be read against the Recommendation
  with no DSP in the way.
- **Call establishment** (`Modec.Handshake`): the V.25 answer sequence
  and the fallback ladder, V.22 §6.3 with the S1 exchange and the 270 ms
  rate switch, both roles, verified by duplex simulation.
  `--mode` says which modes to negotiate and in what order;
  [docs/negotiation.md](docs/negotiation.md) has the decision tree.
- **V.8 and V.8bis** (`Modec.V8`, `Modec.V8bis`): ANSam and the CM/JM/CJ
  menus (`--v8`); CRe, ESr, CL and MS over V.21 HDLC, with the roles
  reversed for the start-up as §9.9.3 requires (`--no-v8bis` skips it).
  A peer with neither gets the classic start-up after 3 s.
- **Listening to the line** (`Modec.Progress`, `Modec.Dtmf`): 60 ms
  frames name which tones are sounding, and their cadence names what it
  means, which in most of the world is the only thing that can. Busy,
  congestion and a special information tone hang the call up with BUSY
  unless `--ignore-busy`. DTMF is Q.24's eight Goertzels, with the
  duration of a tone measured rather than counted in blocks.
- **MNP** (`Modec.Mnp`, `--mnp`): classes 2 to 4 of ITU-T V.42 (10/96)
  Annex A. Go-back-N with a credit window, the timers of A.7.5, and both
  of A.7.2.2's fallbacks including the silent one. Class 3 drops the
  start and stop bits between the modems, worth a fifth of the line;
  class 4 sizes the frames to it. See [docs/mnp.md](docs/mnp.md) and
  [docs/mnp-bench.md](docs/mnp-bench.md).
- **The line** (`Modec.Modem`, `Modec.Pipewire`, `Modec.Baresip`): audio
  through `pw-cat` or raw 8 kHz s16le pipes, or through baresip for SIP;
  data over telnet (BINARY and SUPPRESS-GO-AHEAD negotiated, IAC
  escaped), stdio, or a terminal in raw mode. Hayes AT on the DTE side
  (`--hayes`: ATD, ATA, ATH, ATO, ATZ, AT&F, ATS0, "+++" with its guard
  times, and the CONNECT / RING / NO CARRIER / BUSY result codes), and
  `modec dial NUMBER` for the whole thing in one command.
- **Everything is recorded**: each call writes a WAV, a log of what the
  modem made of it stamped from the start of the call, and a line in
  `recordings/calls.log`; `--record-rx` / `--record-tx` keep a whole
  session, and `modec probe`, `detect`, `progress` and `dtmf` read them
  back through the WAV reader (PCM 8, 16, 24 and 32 bit, and float 32).
  Length fields are refreshed twice a second, so a process killed
  mid-call still leaves a playable file.

Why each of these is built the way it is -- why the burst receiver
decides carrier on whether the band holds a tone rather than on energy,
why the progress window is 60 ms and not 100, what minimodem 0.24 does
to a figures shift -- is in the module headers, which is where it stays
current.

## Limits

- FSK: V.21 tolerates an adjacent channel to about +25 dB, its channels
  being only 470 Hz apart; Bell 103 to beyond +30 dB. Dropouts lose the
  characters they hit, and clock offsets beyond ±3 % fail.
- 2400 bit/s: re-acquisition after a jitter-buffer slip is slow, tens of
  bytes each, and heavy sinusoidal jitter breaks the coherent path.
- 9600 bit/s: delay distortion past 1 ms breaks it where 4800 rides
  through.
- Echo on a V.32 call: a reflection at -26 dB is carried end to end; the
  same path 6 dB louder is not. The canceller is not what decides that.
  It reaches 18 dB of return loss at the louder setting and the call
  still fails, exactly as it failed when the canceller was wired in but
  switched off and removing nothing at all. Whatever breaks 9600 through
  an echo is upstream of the cancelling, and finding it is the next
  piece of work.
- Offline `modec detect` names V.23's backward channel but not its
  forward one: at 1200 bit/s no analysis window can both separate
  1300 Hz from the Bell 103 mark 30 Hz away and stay short enough for a
  continuous-phase carrier to add up over it.

Not yet: V.32 in automode -- it is reached by `--mode v32` or by V.8
choosing it, but it is not in the probe rotation; V.34 or anything else above 9600; SIP/RTP spoken directly
rather than through baresip; ring detection (a sound card carries no
ringing, so ATS0 answers on sustained line energy instead); a native
PipeWire node (pw-cat child processes are used instead); V.22bis guard
tone by default; V.8bis MR/ESi-initiated transactions and the V.8
start-up variants beyond CM/JM/CJ. [SURVEY.md](SURVEY.md) §7 has these
in the order they cost least.

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
cabal run modec -- progress recording.wav                        # dial tone, ringing, busy, congestion, SIT
cabal run modec -- dtmf recording.wav                            # DTMF digits, with timings
cabal run modec -- replay --mode v22bis,v22 recording.wav        # the whole modem over a recorded call
cabal run modec -- replay --mode v22bis,v22 --channel voip --impair impulse=20 recording.wav
# 5-bit text telephone (TTY/TDD): text in, text out, not bytes
printf 'HELLO GA SK' | cabal run modec -- encode --tty45 -o tty.wav
cabal run modec -- decode --tty45 tty.wav                         # --tty50 for the 50 baud line
cabal run modec-bench -- --channel answer --bytes 400             # impairment sweep (also v21, v22low/high, v22bislow/high)
cabal run modec-bench -- v32-survey                               # V.32 against every impairment axis, per rate

# live modem: answer calls arriving on the default PipeWire source, serve telnet on 2323
cabal run modec -- modem --answer --audio-pipewire --listen 2323
# call out: audio through PipeWire, bytes to a telnet host
cabal run modec -- modem --originate --audio-pipewire --connect bbs.example.org --port 23
# pick the PipeWire node explicitly, force Bell 103, skip the handshake
cabal run modec -- modem --answer --audio-pipewire --pw-in usb --mode bell103 --no-handshake --listen 2323
# only the North American modes, best first
cabal run modec -- modem --hayes --audio-pipewire --mode bell212a,bell103 --listen 2323
# V.8: answer with ANSam and read the far end's menu before anything trains
cabal run modec -- modem --answer --audio-pipewire --v8 --listen 2323
# loop two instances through FIFOs with no sound card
scripts/smoke-loopback.sh
# Hayes mode: a terminal program talks AT commands over telnet; ATDT dials with DTMF
cabal run modec -- modem --hayes --audio-pipewire --listen 2323
scripts/smoke-hayes.sh

# SIP: install baresip and copy docs/baresip to ~/.baresip, edit accounts, then just
cabal run modec -- dial +14042820600                              # starts baresip, dials, records
cabal run modec -- dial +14042820600 --mode v21 --listen 2323    # on telnet instead of this terminal
# the long form, for an existing baresip or the answering side
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain sip.provider.example --audio-sip-loop modec --listen 2323
# and from a terminal program: ATDT<number> dials the BBS, ATA answers an incoming SIP call
scripts/smoke-sip.sh
```

`--audio-pipewire` takes the PipeWire defaults; `--pw-in` / `--pw-out`
select an input and an output by node id, node name or any unambiguous
part of either (`--pw-in usb --pw-out analog`), and `--pw-target` sets
both. An ambiguous name is refused with a listing rather than guessed.
With no capture device modec records the playback monitor instead and
says so, which lets the modem hear its own tones (`--pw-monitor` forces
it). If the capture stream stops, it reports NO CARRIER and restarts the
audio, giving up after three attempts. To play with it from a terminal:

```
cabal run modec -- modem --hayes --audio-pipewire --pw-monitor --data-stdio
ATE0          # the terminal already echoes what you type
ATA           # hear the V.8bis CRe and the 2100 Hz answer tone from the speakers
ATH
ATDT5551234   # hear DTMF, then the modem waits for an answer tone
```

Telnet clients see the negotiation as it happens; bytes flow once the
log on stderr says `CONNECT`, and `NO CARRIER` or a failed handshake
ends the process.

## Test fixtures

`test/fixtures/*.wav` were generated with minimodem (`minimodem --tx -f
out.wav -R <rate> [-M mark -S space] 300 < text`); the matching `.txt`
holds the payload. `bell103_ans_8k_noisy.wav` has sox white noise mixed in.
