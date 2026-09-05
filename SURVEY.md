# Survey: existing work for a Haskell software audio modem

Date: 2026-09-05. Scope: Bell 103, V.22, V.22bis, V.8bis link establishment.
Audio side: PipeWire (real hardware) now, SIP/RTP later. Data side: telnet byte streams.

Decisions already taken: all modem DSP is written from scratch in Haskell. Existing
modems below are reference reading and test oracles, not dependencies. Channel model
is the standard telephone band (300–3400 Hz), so the natural internal sample rate is
8 kHz; PipeWire resamples from 48 kHz, and G.711 over RTP is 8 kHz already.

## 1. Existing software modems (reference reading)

| Project | Lang / license | Standards | Status | Why it matters |
|---|---|---|---|---|
| [spandsp](https://github.com/freeswitch/spandsp) (Steve Underwood) | C, LGPL-2.1 | Bell 103, V.21, V.23 (fsk.c); V.22bis tx+rx (v22bis_*.c); V.8 (v8.c); fax modems | Active (pushed 2026-07). v22bis_rx.c header says "work in progress, basically functional, doesn't reliably sync over full signal/noise range; retrain and rate change incomplete" | The only complete open V.22bis receiver. Best single reference for the receiver structure and the training state machine. In Arch repos as `spandsp 0.0.6`. |
| [linmodem](https://github.com/geofft/linmodem) (Fabrice Bellard, 1999) | C, GPL-2 | V.8, V.21, V.23, DTMF, V.22 data pump test, V.34 mod/demod (no echo canceller), V.90 algebraic part | Dormant; Osmocom mirror (gitea.osmocom.org/eloy/linmodem) had build fixes in 2024 | Cleanest small V.8 implementation. Also has a phone-line simulator (echo, amplitude/phase distortion) and an X11 constellation/FFT display. Its V.22 file is a data-pump test, not a full modem. |
| [minimodem](https://github.com/kamalmostafa/minimodem) (Kamal Mostafa) | C, GPL-3 | Bell 103, Bell 202, V.21 (via `--bell103`, arbitrary FSK), RTTY, NOAA SAME, caller ID | Maintained (2024). AUR only on Arch | Demod is FFT-bin energy comparison per bit window, with a bit-sync search. Excellent oracle: generates and decodes Bell 103 WAVs with known payload. |
| [tynsel](https://github.com/kulp/tynsel) | C, MIT | Bell 103 | 2026 activity, tiny (AVR target) | Notch/IIR filter bank at 8 kHz, coefficients from Octave. Author recommends minimodem instead. No shipped WAVs. |
| [lukexor/bell103_demodulator](https://github.com/lukexor/bell103_demodulator) | Rust, GPL-3 | Bell 103 answer channel only | 2019, toy | Decodes 48 kHz mono WAV; README references a `fortune.wav` sample. |
| [jremington/Bell-103-modem-demodulator](https://github.com/jremington/Bell-103-modem-demodulator) | C, GPL-3 | Bell 103 | 2022, toy | Per-bit DFT approach. Uses Audacity to resample to 9600 Hz first. |
| [asterisk-Softmodem](https://github.com/proquar/asterisk-Softmodem) (branch `app_softmodem`) and the copy in [phreakscript](https://github.com/InterLinked1/phreakscript/blob/master/apps/app_softmodem.c) | C, Asterisk module | V.21, V.23, Bell 103, V.22, V.22bis (all via spandsp) | proquar dormant (2018); phreakscript copy maintained | Architecturally the closest thing to this project: PBX audio (G.711 over SIP) ↔ spandsp modem ↔ TCP socket with telnet-style negotiation. Configurable byte size, stop bits, bit order. Read it for the byte-framing and TCP glue, not for DSP. |
| [tcpser](https://github.com/go4retro/tcpser) | C | none (Hayes emulation over TCP) | Maintained forks | No audio at all, but a good reference for the telnet side: detects telnet, negotiates RFC 856 binary for 8-bit transparency, Hayes AT command handling. |
| Commercial: [VOCAL](https://vocal.com/data-modem/v-22/) SIP softmodem, GAO Research | closed | V.22bis, V.8bis, V.34, V.90 | n/a | Proof that "modem over SIP with G.711" is a product category. Their V.22bis and V.8bis product pages are useful short summaries. |

Nothing modem-related exists in Haskell. GitHub and Hackage searches for Haskell + Bell 103 / V.21 / V.22 / AFSK / AX.25 found nothing.

### What spandsp's V.22bis receiver actually does (from `v22bis_rx.c`)

Worth knowing before designing from scratch, since it is the one open design that works:

- Input at 8 kHz goes into a polyphase RRC pulse-shaping/bandpass filter with 12 phase-shifted coefficient sets (separate coefficient files for the 1200 Hz and 2400 Hz carriers).
- Processing at T/2 (1200 samples/s for the 600 baud symbol rate). Every second T/2 sample it: runs symbol timing (`symbol_sync`, using the last 3 equalizer-buffer samples), runs a 17-tap complex fractionally spaced equalizer (LMS, delta 0.25), slices to the nearest constellation point (16-way for 2400 bps, 4-way rotated by 45° for 1200 bps), decision-directed carrier tracking (`track_carrier`), equalizer update (`tune_equalizer`), differential phase decode via `phase_steps`, descramble (1 + x^-14 + x^-17), output bits.
- Receiver training stages: SYMBOL_ACQUISITION → UNSCRAMBLED_ONES (calling side) or SCRAMBLED_ONES_AT_1200 → optional WAIT_FOR_SCRAMBLED_ONES_AT_2400 → NORMAL_OPERATION, with PARKED for failure. Transmit side: INITIAL_SILENCE / INITIAL_TIMED_SILENCE → U11 (unscrambled ones) → U0011 (S1) → S11 (scrambled ones at 1200) → S1111 (scrambled ones at 2400) → NORMAL.
- During normal operation it keeps looking for the S1 pattern (alternating 00/11 dibits) which signals a retrain request.

Reading spandsp does not taint a from-scratch reimplementation in a practical sense, but keep the Haskell code independently written rather than transliterated if the LGPL matters to you.

## 2. V.8bis

- No open-source V.8bis implementation was found. spandsp implements V.8 only (`v8.c`, 1417 lines). linmodem implements V.8 only. Search results tie V.8bis to V.34 products, all closed.
- Spec: ITU-T V.8bis (11/2000), in force; earlier editions 08/96 and 09/98. ITU-T V-series recommendations are free downloads from itu.int (V.8, V.8bis, V.21, V.22, V.22bis, V.25 all fetched fine during this survey as PDF links).
- Protocol summary: initiating station signals (CRe, MRe, ESi) are dual tones 1375 + 2002 Hz for 400 ms followed by 100 ms of a single tone (400 Hz for CRe, 650 Hz for MRe); responding signals (CRd, MRd, ESr) use 1529 + 2225 Hz. Tolerances 250 ppm frequency, 2 % duration. Capabilities and mode-select messages are then sent at 300 bps on V.21 channels with HDLC-style framing. V.8bis is optional and is designed so a station that does not answer the CRe falls through to classic V.25 answer tone / V.8 / legacy handshakes. For Bell 103 and V.22 peers, expect to need the legacy handshake regardless.
- Patent note (not legal advice): V.8bis dates from 1996, so any patents covering it would have expired around 2016–2020. V.34 patents were the reason spandsp never did V.34, and those are also expired now.
- The telephony-tone detector you build for V.8bis (dual-tone detection with duration qualification) is the same machinery as the 2100/2225 Hz answer-tone detector and the S1 detector, so it fits the from-scratch plan.

## 3. Spec quick facts (for the from-scratch design)

| Standard | Rate | Modulation | Originate tones / carrier | Answer tones / carrier | Notes |
|---|---|---|---|---|---|
| Bell 103 | 300 bps, 300 Bd | FSK, async | space 1070, mark 1270 Hz | space 2025, mark 2225 Hz | Answer tone is 2225 Hz mark itself. 26.67 samples/bit at 8 kHz. No spec document; Wikipedia plus spandsp's `preset_fsk_specs` are the references. |
| V.21 | 300 bps | FSK | 1180/980 Hz | 1850/1650 Hz | Same structure as Bell 103, different tones. Cheap to add; also the V.8/V.8bis message channel. |
| V.22 / Bell 212A | 1200 bps, 600 Bd | 4-DPSK (dibits) | carrier 1200 Hz | carrier 2400 Hz | Scrambler 1 + x^-14 + x^-17. V.22 answer tone 2100 Hz plus optional 1800 Hz guard tone; Bell 212A uses 2225 Hz answer tone. Both directions duplex via the two carriers. |
| V.22bis | 2400 bps, 600 Bd (fallback 1200) | 16-QAM (differential quadrant + 2 amplitude bits) | 1200 Hz | 2400 Hz | Handshake: answer tone → unscrambled ones (U11) → S1 (unscrambled 00/11 dibits, 100 ± 3 ms) to signal 2400 capability → scrambled ones at 1200 → scrambled ones at 2400 → data. Absence of S1 means fall back to V.22. |

Answer tone (V.25): 2100 Hz for 2.6–4.0 s, optionally with phase reversals every 450 ms (V.25 echo-canceller disabling). Bell: 2225 Hz.

## 4. Haskell infrastructure worth reusing

Everything checked on Hackage on 2026-09-05.

### Audio I/O (PipeWire)

| Option | Last release | Assessment |
|---|---|---|
| Spawn `pw-cat` / `pw-record` / `pw-play` as a child process with raw `s16` mono at 8 kHz on stdin/stdout (`pw-cat --record --rate 8000 --format s16 --channels 1 -`) | pipewire 1.6.8 installed | Zero bindings, PipeWire does the resampling, and the modem core sees a plain byte pipe identical to a WAV file or an RTP stream. Recommended for the MVP. Latency is a few tens of ms, irrelevant at 300–2400 bps. |
| [pipewire.hs](https://github.com/TristanCacqueray/pipewire.hs) (Tristan de Cacqueray) | Not on Hackage; last push 2024-07; no license file detected | Experimental inline-c bindings, small subset of libpipewire. Examples cover playing a tone and a file; capture would need adding. Good later option for a native graph node (so the modem shows up as a PipeWire node that can be linked to a SIP client). |
| [pulse-simple](https://hackage.haskell.org/package/pulse-simple) | 2012 | Blocking read/write over libpulse. The Simple API has not changed since, and pipewire-pulse serves it. Second-simplest path after the child process. |
| [alsa-pcm](https://hackage.haskell.org/package/alsa-pcm) | 2025-11 | Maintained (Henning Thielemann). Works through PipeWire's ALSA plugin. More setup than needed. |
| [jack](https://hackage.haskell.org/package/jack) | 2025-05 | Maintained. Callback model via pipewire-jack, lowest latency, but callbacks in Haskell need care. Overkill here. |
| portaudio | 2014 | Stale, skip. |

### SIP / RTP (later)

- [mediabus-rtp](https://hackage.haskell.org/package/mediabus-rtp) and mediabus (2017): conduit-based RTP parser with G.711 support. Stale but small.
- [hasip](https://github.com/ibeljutins/hasip): GitHub-only SIP library on WAI, not on Hackage.
- Recommendation: do not embed SIP. RTP + G.711 (A-law/µ-law) is a few dozen lines and worth writing yourself; SIP signalling is not. Let baresip, Linphone, Asterisk or FreeSWITCH own the SIP leg and hand audio over via PipeWire (baresip and Linphone both have PipeWire/Pulse audio backends), or accept raw RTP from a PBX `rtp` dialplan leg. The asterisk-Softmodem approach (be an Asterisk app) is the other proven pattern.

### Telnet

- [libtelnet](https://hackage.haskell.org/package/libtelnet) (2021) binds the C libtelnet; works but drags in a C dependency for a small protocol.
- Recommendation: hand-roll IAC handling (RFC 854) plus BINARY (RFC 856), SUPPRESS-GO-AHEAD and ECHO negotiation on top of `network`. That is all tcpser and asterisk-Softmodem do.

### WAV files, streaming, DSP helpers

- WAV: [wave](https://hackage.haskell.org/package/wave) (2026-01, Mark Karpov) for headers; read samples with `Data.Vector.Storable` via `bytestring`. hsndfile (2015) is stale.
- Streaming: [streamly](https://hackage.haskell.org/package/streamly) 0.11.1 (2026-05) for chunked, backpressured pipelines; conduit 1.3.6 also fine. Plain functions over `Vector Double` chunks with explicit state are enough for the modem core and make it easier to test.
- DSP: [dsp](https://hackage.haskell.org/package/dsp) 0.2.5.2 (2025) has Kaiser FIR design, IIR cookbook, FFT, frequency-peak interpolation, all pure list-based Haskell (slow, fine for offline filter design at startup or in a script). [vector-fftw](https://hackage.haskell.org/package/vector-fftw) (2025) binds FFTW if an FFT is ever needed; these modems do not need one (Goertzel / quadrature correlators suffice).
- Prior Haskell SDR art for structure ideas only: [adamwalker/sdr](https://github.com/adamwalker/sdr) (Pipes-based, BSD-3, 2023; FIR, decimation, FM demod, OpenGL waterfall) and [composable-sdr](https://github.com/mryndzionek/composable-sdr) (Streamly plus liquid-dsp FFI, 2024).

## 5. Test material

| Source | Contents | Use |
|---|---|---|
| minimodem (build from AUR or source) | Generates Bell 103 / V.21 WAVs with known payload, either channel, any sample rate; also decodes | Primary oracle for the FSK path. Encode → decode round trips with known text, plus cross-check decoding of our own modulator. |
| spandsp `tests/v22bis_tests` and `tests/fsk_tests` | Full V.22/V.22bis handshake plus data, both parties, with the built-in line model (noise, level) | Only open source that can generate V.22bis signal with known payload. Also the only way to test our V.22bis against an independent implementation without hardware. |
| [Gough Lui, "Sounds of Dialup Modems"](https://archive.goughlui.com/legacy/soundofmodems/index.htm) | Real hardware (TI-based Aztech modem): `v21-300bps.mp3`, `v22b-2400bps.mp3`, plus V.32bis/V.34/V.90/fax | Real handshakes for tone/timing detectors. MP3 at telephone bandwidth is fine for this. Payload unknown, so handshake-detection tests only. |
| [archive.org: ALL Old Modem Sounds (300 baud to 56K)](https://archive.org/details/youtube-ckc6XSSh52w) | Bell 103, V.22bis, V.32bis, V.34, V.90, V.92 from a Conexant softmodem forced via `AT+MS`; mp4/ogv | Same use as above, but the uploader notes timing is slightly off because it is a softmodem. |
| Bell 103 hobby decoders above | lukexor references `fortune.wav` (48 kHz, answer channel) | Small known-payload sample if the file is still in the repo history. |
| Real hardware | Any two Hayes modems, or one modem plus this project through a line simulator / ATA | Eventually mandatory for V.22bis timing and level robustness. |

For payload-known Bell 103 test files, generate them with minimodem rather than hunting recordings: online recordings are handshake demos, and the ones that carry data carry unknown data.

## 6. Suggested from-scratch architecture

```
PipeWire (pw-cat pipe | pipewire.hs later)        RTP/G.711 (later)        WAV file
            \                     |                  /
             --- Samples: Vector Int16 @ 8 kHz mono chunks ---
                                  |
                  Modem core (pure, explicit state, per chunk)
                    Tone detectors: 2100/2225 answer tone, V.8bis dual tones, S1
                    Bell 103 / V.21: complex mixers at mark/space + LPF, noncoherent
                       energy compare, bit-sync on transitions, async 8N1 deframer
                    V.22/V.22bis: mix to baseband at 1200/2400, RRC matched filter,
                       T/2 fractionally spaced LMS equalizer, Gardner or
                       decision-directed timing, decision-directed carrier PLL,
                       differential decode, descrambler, async deframer
                    Handshake state machines (originate / answer, per standard)
                    Modulators: mirror of each, plus scrambler and pulse shaping
                                  |
                  Bytes: ByteString chunks  <-->  Telnet (IAC, BINARY) over TCP
                                  |
                  Optional Hayes AT layer (tcpser semantics) for retro clients
```

Keep the DSP core free of IO so the same code runs against WAV fixtures in the test suite and against PipeWire at runtime. Keep sample rate a parameter but standardise on 8 kHz internally; that also makes the eventual G.711/RTP path a no-op conversion.

Suggested order: tone detector and Bell 103 answer-channel decoder against minimodem output → Bell 103 modulator and full-duplex loop → telnet bridge → V.22 (4-DPSK, no amplitude bits) against spandsp test output → V.22bis 16-QAM and S1 handling → V.8bis capabilities exchange on top of the V.21 channel code.

Progress (2026-09-05): FSK modem, channel simulator, tone detection, Bell 103 / V.21 / V.22 / V.22bis handshakes, live modem with PipeWire/telnet I/O, and the V.22/V.22bis data pump (validated against spandsp at 1200 and 2400 bit/s) are done (see README). Next: V.8bis, SIP/RTP, Hayes AT layer.

V.22bis constellation (Figure 2/V.22bis, confirmed from the rendered PDF page and identical to spandsp's table): first quadrant 00 = (1,1), 01 = (3,1), 10 = (1,3), 11 = (3,3); the other quadrants are that pattern rotated by 90° per quadrant. The 1200 bit/s (V.22-compatible) points are the 01 points, i.e. (3,1) rotated. Quadrant changes per Table 1: 00 = +90°, 01 = 0°, 11 = +270°, 10 = +180°.

spandsp oracle recipe: clone github.com/freeswitch/spandsp, `autoreconf -fi && ./configure && make`, then `make -C spandsp-sim LIBS=-lfftw3` and `make -C tests v22bis_tests LIBS="-lfftw3 -lsndfile"`; run `tests/v22bis_tests -l -b 1200` or `-b 2400` (it runs until killed; kill it after a minute, and never with `pkill -f` on a pattern that matches your own shell) and split `tests/v22bis.wav` with `sox --ignore-length v22bis.wav -c 1 out.wav remix 1 trim 0 40` (channel 1 = calling modem, low channel; remix 2 = answering modem). Its data source is the ITU O.152 2047-bit PRBS (x^11 + x^9 + 1).

## 7. Documents to fetch

All free from ITU-T: V.8, V.8bis (11/2000), V.21, V.22 (11/1988), V.22bis (11/1988), V.25 (answer tone), V.14 (async-to-sync for V.22 start/stop bit handling). Bell 103 and 212A have no public standard; use Wikipedia, spandsp's `preset_fsk_specs`, and the V.22 Annex on Bell 212A compatibility.
