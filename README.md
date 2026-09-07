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

- Bell 103, V.21 and V.23 duplex asynchronous FSK modulator and
  demodulator,
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
- V.23 duplex (`--mode v23`): 1200 bit/s from the answering modem on
  1300/2100 Hz, 75 bit/s back from the caller on 390/450 Hz. The only
  asymmetric mode here, and the one viewdata boards answer with. Calling
  one needs nothing special -- the 1300 Hz forward mark is unambiguous --
  but answering one means measuring 390 Hz, which is one bin from the
  V.8bis CRe tone at 400 Hz, so a V.23 answerer trades V.8bis for it
  (`withModes` does the swap). Offline `modec detect` names the backward
  channel but not the forward one: at 1200 bit/s no analysis window can
  both separate 1300 Hz from the Bell 103 mark 30 Hz away and stay short
  enough for a continuous-phase carrier to add up over it.
- Text telephone, 5-bit Baudot (`--tty45` / `--tty50`, `Modec.Baudot`):
  the TTY/TDD line deaf and hard-of-hearing users have had on the PSTN
  since 1964, ITU-T V.18 Annex A and normatively ANSI/TIA-825. 1400 Hz
  mark, 1800 Hz space, 45.45 baud (50 outside North America), one start
  bit, five data bits and at least one and a half stop bits. Offline
  `encode`/`decode` carry text, not bytes: `Modec.Baudot` holds V.18
  Table A.1 and the ASCII folding of Table A.2, tracks the LTRS/FIGS
  shift, opens with LTRS and re-sends the shift every 72 characters, and
  deliberately does *not* unshift on space -- that is the RTTY
  convention, and minimodem's `tdd` mode applies it by default, so a
  cross-check against minimodem needs `-u 0`.
  Text is exact through the telephone channel from 30 dB SNR down to
  4 dB at both rates. The 45.45 and 50 baud lines are separate modes,
  not a tolerance: 10 % apart, where this family of receivers gives up
  around 3 %.
  Validated against minimodem 0.24 in both directions at both rates:
  its `tdd` preset on its own defaults reads 35 of 35 characters from
  our transmitter with the clock 0.0 % fast, and we read its transmitter
  byte for byte including CR and LF. Two findings came out of that.
  We were sending V.18's *minimum* of 1.5 stop bits, which minimodem's
  preset requires 2.0 of -- it lost 3 characters in 35 and read the
  clock 3.9 % fast -- so the transmitter now sends 2 and the receiver
  still asks for no more than a mark stop bit, since a far end sending
  1.5 is correct. And minimodem 0.24 applies unshift-on-space
  unconditionally (the `-u` switch does not exist in that release), so
  after a space it stops re-sending FIGS: `HELLO GA 50 BAUD SK` comes
  back from it as `HELLO GA 50 ?-7$ (`, which is precisely the figures
  column. V.18, TIA-825, Asterisk and spandsp all agree there is no
  unshift-on-space, and real TDD traffic depends on it -- `GA 555 1212`
  has to stay in figures across its spaces -- so this is one to know
  about rather than one to match.
- The carrierless receiver (`fskBurstDeframer`): a text telephone sends
  no carrier at all between characters, so a burst can begin with the
  start bit itself and there is no idle mark to hunt in. `fskDeframer`
  loses a third of the characters of a clean burst on that alone, so
  this is a sibling rather than a flag on it, which also keeps five
  measured and tuned modes out of the blast radius. It acquires on a
  silence-to-space onset as well as on a mark-to-space crossing, and it
  decides carrier on whether the band holds a *tone* -- the mean of
  |decide| is one half on noise whatever the noise power, and one on a
  tone -- rather than on energy against a threshold, which cannot be
  made to work: every version of a tracked noise floor either latches on
  after one spike or seeds itself shut. Energy still decides whether a
  character already under way has a line to run on, because |decide|
  dips at every bit transition. Getting those two roles the wrong way
  round turned 30 characters into 55.
- Call establishment (`Modec.Handshake`): V.25 answer sequence with
  Bell 103, V.21, V.23, Bell 212A, V.22 and V.22bis on both sides, verified by
  duplex simulations in the test suite. `--mode` chooses which of them
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
  270 ms rate switch and the 32-ones completion) in the handshake and
  the live modem. Automode probes V.22 first, then V.21, then Bell 103,
  accepts a Bell 103 caller at any point, and falls back to 1200 bit/s
  with a V.22-only peer. `--mode v22` disables 2400 on our side.
- V.8bis capabilities exchange (`Modec.V8bis`, `Modec.Hdlc`): after the
  billing delay the answerer sends CRe (dual tone 1375 + 2002 Hz, then
  400 Hz), a V.8bis caller replies with ESr and a CL message over V.21
  (HDLC frames with the ISO 3309 FCS, synchronous 300 bit/s), the answerer
  selects the best common mode with MS, and the V.25 start-up follows with
  the roles reversed as §9.9.3 requires (the MS receiver becomes the
  answering modem). Peers without V.8bis get the classic start-up after
  3 s. `--no-v8bis` skips it.
- Call progress tones (`Modec.Progress`, `modec progress`): dial tone,
  ringing, busy, congestion, the fax calling tone, the answer tone and
  the special information tone that introduces a recorded announcement.
  Frames of 60 ms name which tones are sounding; the cadence of those
  frames names what they mean, which is the only thing that can, since
  ITU-T E.180 country practice is one 425 Hz tone for dial tone,
  ringing, busy and congestion alike and only the rhythm separates
  them. North America's own pairs (350+440, 440+480, 480+620) and the
  UK's double ring are read as well. A call modec places listens until
  the modems connect and reports what it heard; busy, congestion and a
  special information tone hang the call up with BUSY, since all three
  mean the call was refused. `--ignore-busy` stays on the line instead.
  Robust to 3 dB SNR and to 30 dB below full scale. Across the 221
  recorded calls in `recordings/` it reports 36 ringings and 159 answer
  tones and not one busy: the single false positive it started with was
  a stretch of speech that produced two bursts near 400 Hz with the
  spacing of congestion, which is why busy and congestion ask for a
  third burst where ringing is content with two. The three segments of
  a special information tone are reported as measured, not named:
  Telcordia's table gives each combination of frequencies and durations
  a meaning, the published copies of it disagree with one another, and
  nothing here has yet heard a real one to check a name against.
- DTMF detection (`Modec.Dtmf`, `modec dtmf`): the eight-Goertzel block
  receiver of ITU-T Q.24, with its level, twist, relative-peak and
  fraction-of-total-power tests. The last of those is what rejects
  speech, since a vowel really does have energy at 770 and 1336 Hz but
  spends most of its power elsewhere. How long a tone lasted is
  measured rather than counted in blocks, and has to be: Q.24 wants
  40 ms accepted and 23 ms rejected, which are 1.3 blocks apart, so
  whatever number of blocks is demanded, one of the two requirements
  fails at some alignments of the tone against the block grid. The
  Goertzel is linear in coverage, so summing a run's levels and
  dividing by the largest of them measures the tone to a few
  milliseconds, and both requirements then hold at every alignment.
  Digits survive the telephone channel to 3 dB SNR.
- Hayes AT command mode (`Modec.Hayes`, `--hayes`): ATD (digits dialled as
  DTMF, then originate), ATA, ATH, ATO, ATZ, AT&F, ATE/V/Q, ATI, ATS0
  (auto-answer on a sustained calling signal), "+++" with guard times;
  result codes OK, CONNECT 300/1200/2400, RING, NO CARRIER, BUSY, ERROR.
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
- MNP error correction (`Modec.Mnp`, `Modec.MnpFrame`, `--mnp`): classes 2,
  3 and 4 of ITU-T V.42 (10/96) Annex A, which is MNP de-branded. Frames
  the data, checks it with CRC-16/ARC or the HDLC check sequence, and asks
  again for what the line damaged; go-back-N with a credit window, the
  timers of A.7.5, and both of A.7.2.2's fallbacks, including the silent
  one that carries on unprotected when nothing answers. Class 3 drops the
  start and stop bits between the modems, worth 21 % of the line on a
  256-octet frame and 31 % on a 16-octet one; class 4 shortens the headers
  and sizes the frames to the line. Through the simulator it delivers every
  byte where the bare link is damaging 3 % of them, and comes up at 4 dB
  where better than one byte in ten arrives damaged. See
  [docs/mnp.md](docs/mnp.md), and
  [docs/mnp-bench.md](docs/mnp-bench.md) for what is confirmed against real
  hardware, what is not, and the order to test it in on the bench.
- One command to place a call: `modec dial NUMBER` starts baresip if it
  is not already up, finds the SIP domain in `~/.baresip/accounts`, dials,
  and hands the call to the terminal in raw mode. `--listen PORT` puts it
  on telnet instead, `--stay` keeps the AT prompt when the call ends.
- Per-call recordings: every call, dialled or answered, writes
  `recordings/<date>-<number>.wav` alongside a `.log` of what the modem
  made of it, stamped in seconds from the start of the call, and a line
  in `recordings/calls.log`. `--record-dir` moves them, `--no-record`
  turns them off.
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
cabal run modec -- progress recording.wav                        # dial tone, ringing, busy, congestion, SIT
cabal run modec -- dtmf recording.wav                            # DTMF digits, with timings
# 5-bit text telephone (TTY/TDD): text in, text out, not bytes
printf 'HELLO GA SK' | cabal run modec -- encode --tty45 -o tty.wav
cabal run modec -- decode --tty45 tty.wav                         # --tty50 for the 50 baud line
cabal run modec-bench -- --channel answer --bytes 400             # impairment sweep (also v21, v22low/high, v22bislow/high)

# live modem: answer calls arriving on the default PipeWire source, serve telnet on 2323
cabal run modec -- modem --answer --audio-pipewire --listen 2323
# call out: audio through PipeWire, bytes to a telnet host
cabal run modec -- modem --originate --audio-pipewire --connect bbs.example.org --port 23
# pick the PipeWire node explicitly, force Bell 103, skip the handshake
cabal run modec -- modem --answer --audio-pipewire --pw-in usb --mode bell103 --no-handshake --listen 2323
# only the North American modes, best first
cabal run modec -- modem --hayes --audio-pipewire --mode bell212a,bell103 --listen 2323
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
