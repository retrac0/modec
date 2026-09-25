# Telebit PEP: what is documented, and whether it can be built

PEP — Packetized Ensemble Protocol — is the proprietary multicarrier
modulation the Telebit TrailBlazer family used from 1985 to the
mid-1990s. It was never an ITU Recommendation, never second-sourced, and
never published as a specification. Nothing in it was written down the
way V.32bis was written down.

What *was* written down is four patents, and they turn out to be much
better than the usual patent. The numbers in them match the numbers
independently reported for the shipped product, which is the strongest
evidence available that they describe the real thing rather than a
lawyer's abstraction of it.

This document collects every source worth having, states which
parameters can be pinned down and which cannot, and answers the
feasibility question.

## The sources, ranked by what they are worth to an implementer

### Tier 1 — the patents (the only spec-grade material)

All four are expired. Full text and drawings are free on Google Patents;
PDFs mirror from `patentimages.storage.googleapis.com/pdfs/US<number>.pdf`.

| Patent | Inventor | Filed | Issued | What it gives you |
|---|---|---|---|---|
| [US4438511](https://patents.google.com/patent/US4438511A/en) — Packetized ensemble modem | Paul Baran | 1980-11-10 | 1984-03-20 | The *predecessor* design: 64 carriers at 37.5 Hz, 5 bits each, 2/75 s epochs, 640-bit packets, 24-bit CRC, 2-bit sequence + 2-bit ack. Not PEP, but PEP's framing vocabulary comes from here, and 4679227 incorporates its modulator/demodulator by reference. |
| [US4679227](https://patents.google.com/patent/US4679227A/en) — Ensemble modem structure for imperfect transmission media | Dirk Hughes-Hartogs | 1985-05-20 | 1987-07-07 | **The core document.** Carrier plan, FFT size, epoch and guard timing, the two-tone timing recovery, the eight-step probe handshake, the waterfilling bit-loading algorithm, the constellation set, the decision-directed tracking loop, the duplex arbitration, even the hardware (68000 + TMS320). |
| [US4731816](https://patents.google.com/patent/US4731816A/en) | Hughes-Hartogs | 1987-01-12 | 1988-03-15 | Continuation. Same disclosure, claims aimed at the guard-time waveform and the duplex allocation. |
| [US4833706](https://patents.google.com/patent/US4833706A/en) | Hughes-Hartogs | 1988-01-05 | 1989-05-23 | Continuation. Claims aimed at the tracking system and timing recovery. |
| [EP0224556B1](https://patentimages.storage.googleapis.com/dc/63/76/efc235571e4685/EP0224556B1.pdf) / [WO1986007223](https://patentscope.wipo.int/search/en/WO1986007223) | Hughes-Hartogs | 1986-05-05 | — | European family member of 4679227. Cleaner typesetting than the OCR'd US scans. |

The Hughes-Hartogs bit-loading algorithm in 4679227 is also covered in
every DSL textbook written since 1995 — it is the ancestor of ADSL's
DMT bit loading, and is where the phrase "Hughes-Hartogs algorithm"
comes from. That literature is a cross-check on the algorithm, though
not on PEP's wire format.

### Tier 2 — vendor manuals (behaviour, not format)

On bitsavers under `/communications/telebit/` (use the
`bitsavers.trailing-edge.com` mirror; `www.bitsavers.org` 403s a bare
`curl`):

- [T2500 Reference Manual, Rev. D, 1991](http://bitsavers.trailing-edge.com/communications/telebit/t2500/90100-03D_Telebit_T2500_Reference_Manual_1991.pdf) (252 pp) — the fullest PEP-era manual.
- [T1600 Reference Manual, 1990](http://bitsavers.trailing-edge.com/communications/telebit/t1600/90156-01A_Telebit_T1600_Reference_Manual_1990.pdf)
- [T3000 Reference Manual, 1991](http://bitsavers.trailing-edge.com/communications/telebit/t3000/90201-01A_Telebit_T3000_Reference_Manual_1991.pdf)
- [T3000 + WorldBlazer Modem Reference, 90238-01](http://bitsavers.trailing-edge.com/communications/telebit/90238-01_Modem_Reference_Manual_for_the_Telebit_T3000_and_WorldBlazer_Family_of_Products.pdf) — the TurboPEP one.
- [WorldBlazer Addendum](http://bitsavers.trailing-edge.com/communications/telebit/worldblazer/90227-01_Telebit_Worldblazer_Modem_Addendum.pdf), [Standalone WorldBlazer User's Guide 1992](http://bitsavers.trailing-edge.com/communications/telebit/worldblazer/90257-01_Telebit_Standalone_WorldBlazer_Users_Guide_1992.pdf)

These document no signal formats at all. What they *do* document, and
what makes them valuable, is the observable state a real modem will
report — see "Why a real Telebit is an unusually good oracle" below.

### Tier 3 — community and secondary

- **"Telebit modem questions answered"**, comp.dcom.modems — a
  Telebit-authored Q&A posted to Usenet. The only public source for the
  two-epoch structure: *"the modem operates at 7.35 and 88.26 baud … in
  interactive mode the modem sends data using 11 msec packets (88.26
  baud), each containing 15 bytes of data; in file transfer mode it uses
  136 msec packets (7.35 baud) containing 256 bytes."* Also the source of
  the "DAMQAM — Dynamically Adaptive Multicarrier QAM" name and the
  "511 channels at 2, 4 or 6 bits" phrasing everyone else copies.
  ([Google Groups](https://groups.google.com/g/comp.dcom.modems/c/7gxaiOs_P14) — rate-limits aggressively.)
- [NSRC, "Telebit PEP protocol optimalization"](https://nsrc.org/archives/lowcost_tools/telebit.html) — the operational
  view of PEP's packet sizes: `S120` selects micro/short/long packets and
  auto-negotiates; `J6 S36` prepends a protective tone to each PEP packet
  against clipping; `S121` enables echo-suppressor compensation for long
  delay circuits.
- [ESR's Telebit FAQ](http://www.catb.org/~esr/faqs/trailblazer.txt) — UUCP/Unix configuration only, no protocol content. (HTTPS cert on catb.org is wrong; fetch over HTTP.)
- J. A. C. Bingham, *"Multicarrier Modulation for Data Transmission: An
  Idea Whose Time Has Come"*, IEEE Comm. Mag. 28(5), May 1990, 5–14,
  doi:10.1109/35.54342. Bingham was **at Telebit**. Confirms 7.8125 Hz as
  the TrailBlazer's carrier separation. Paywalled; circulates as an
  exhibit in ADSL IPR filings.
- [Wikipedia: Telebit](https://en.wikipedia.org/wiki/Telebit) — good on product history, **wrong on the baud rate** (says 6 baud; see below).
- Eric Smith, "A Brief History of Telebit", `brouhaha.com/~eric/personal/telebit.html` — currently NXDOMAIN, try the Wayback Machine.

### What does not exist

Worth stating plainly, because the absence is the whole feasibility
problem:

- No published PEP specification, and no standards-body document.
- No second implementation, ever. PEP was never licensed to another
  modem maker; there is no interoperating peer except a Telebit.
- No firmware ROM dump, disassembly, or preservation project.
- No prior reverse-engineering effort, no captured-signal corpus, no
  analysis tooling. A YouTube recording of a PEP handshake is the extent
  of publicly available PEP audio.

## The parameters that can actually be pinned down

From US4679227 unless noted. These are *documented*, not inferred:

| Parameter | Value |
|---|---|
| Sampling | 8 kHz, 125 µs, ~4 kHz Nyquist band |
| Transform | 1024-point FFT / IFFT |
| Carriers | 512, indexed f₀…f₅₁₁ across the 4 kHz band (FIG. 1) |
| Carrier spacing | 7.8125 Hz (4000/512) |
| Symbol duration T_E | 128 ms (1024 samples) |
| Guard T_PH | 8 ms (64 samples), formed by repeating the symbol's first 8 ms *after* it |
| Epoch | 136 ms → **7.3529 baud**; guard costs 6.25% |
| Receive window | 128 ms starting at T₀ + 8 ms, i.e. the last 128 ms of the 136 |
| Constellations | 0, 2, 4, 5, 6 bits per carrier (FIG. 5) |
| Constellation shapes | 2b: QPSK at (±1,±1); 4b: 16-QAM on ±1,±3; 5b: 32-cross (corners of the 6×6 trimmed, outer points (±3,±5),(±5,±3)); 6b: 64-QAM on ±1…±7 |
| Relative power for equal BER | 1.00 / 10.0 / 20.0 / 32.75 for 2/4/5/6 bits, at E_b/N₀ = 0.5 / 2.50 / 4.00 / 5.46 |
| Bit-loading range signalled | 0–15 bits and 0–63 dB per carrier |
| Power constraint | total ≤ P₀; 0 dBR = −9 dBm |
| Timing tones | 1437.5 Hz and 1687.5 Hz at −3 dBR, zero relative phase (bins 184 and 216 — both land exactly on the 7.8125 Hz grid, 32 bins = 250 Hz apart) |
| Timing resolution | the 250 Hz spacing gives 11° of differential phase per 125 µs sample offset |
| Probe comb | all 512 frequencies at −27 dBR |
| Bootstrap link | DBPSK at −28 dBR: 180° = "this carrier supports 2 bits", 0° = it does not |
| Typical usable carriers | "300 to 400 frequency components", ≈600 bits/epoch bootstrap rate |
| Frequency offset tolerance | ±7 Hz, corrected by SSB modulation of the quadrature tones |
| Tracking | decision-directed, per-carrier ±0.1 dB and ±1.0° from four-quadrant error counts around each constellation point |
| Duplex | half-duplex with turnaround; each side sends between I and N epochs per turn, I sent even with no data to keep sync; N may differ per direction |
| ARQ | CRC per packet, retransmit until correct; FEC named as an alternative |

The eight-step link-setup sequence (dial → answer two-tone + answer comb
→ noise floor FFT → originate two-tone + originate comb → DBPSK
two-bit map originate→answer → DBPSK two-bit map both directions → bit
and power table for one direction over that link → table for the other)
is given step by step in the patent and is implementable as written.

### Reconciling the numbers you will see quoted

Three figures circulate and they do not agree. The patent wins:

- **7.35 baud, not 6 baud.** 136 ms epochs give 7.3529 baud, and the
  Telebit Usenet Q&A independently quotes 7.35. Wikipedia's "6 baud"
  and the "512 × 6 bits × 6 baud = 18432 bps" arithmetic derived from
  it are wrong. The patent's own ceiling, 22,580 bps, is
  512 × 6 × 7.353 — consistent.
- **511 vs 512.** FIG. 1 numbers the carriers f₀…f₅₁₁. The marketing
  "511 frequency points" is those minus DC. The T3000/WorldBlazer manual
  says "512 frequency points" for `S71`/`S73`. Same thing.
- **18,000 / 19,200 bps.** DTE-side figures including compression, not
  line rate. 256 payload bytes per 136 ms epoch is 15,059 bps of user
  data; over 350–400 usable carriers that is 5–6 bits on most of them,
  which is exactly what the design predicts on a good line.

### What is inference, not documentation

Flagged so nobody mistakes it for a source later:

- **The 88.26-baud interactive epoch is undocumented.** 88.26 / 7.35 =
  12.00, so the short epoch is almost certainly the long one divided by
  twelve: ~11.33 ms symbols on a 93.75 Hz carrier grid, ~33 usable
  carriers across 300–3400 Hz, 15 bytes + CRC ≈ 136 bits per packet ≈ 4
  bits per carrier. Self-consistent, and nothing confirms it. Whether it
  is a separate FFT size, a decimated subset of the same grid, or
  something else is not recorded anywhere public.
- Whether the shipped TrailBlazer used all 512 bins or only the
  in-band ones is not stated; "300 to 400 frequency components" support
  two bits, and 3100 Hz / 7.8125 Hz = 397, so the usable set is the
  passband and the outer bins are dead weight the probe discovers.

### What is documented *nowhere*

This is the list that decides the feasibility answer:

- Bit-to-constellation-point labelling for each of the 2/4/5/6-bit
  constellations. Shapes are drawn; the mapping is not.
- The order bits are drawn from the stream and assigned across carriers.
- Any scrambler, and its polynomial and seed.
- The packet header: sequence numbers, ack/selective-reject encoding,
  window size, where payload length lives.
- The CRC-16 polynomial and its position. (Manuals confirm CRC-16
  exists; Baran's earlier design used a 24-bit CRC, so the older patent
  does not answer it.)
- The encoding of the bit/power allocation tables exchanged in steps
  7–8 — the field widths are given (0–15 bits, 0–63 dB) but not the
  packing.
- The channel-reversal negotiation on the wire.
- Everything before the probe: PEP's answer-tone sequence, and how PEP
  is recognised and selected against V.32/V.22bis in the same handshake.
- The protective tone (`J6 S36`) and echo-suppressor compensation
  (`S121`) waveforms.
- **TurboPEP entirely.** All that is public is "same modulation, up to
  7 bits per baud, 23,000 bps". No patent covers it.

## Feasibility

Two different questions hide inside "can you implement PEP", and they
have opposite answers.

### Building a DMT modem faithful to the patents — yes, clearly

Everything needed for a working transmitter and receiver pair is in
US4679227 at implementable detail. Nothing in it is hard for this
project in particular:

- A 1024-point real FFT/IFFT is the only new DSP primitive, and PEP
  needs no equalizer, no echo canceller, no Viterbi decoder, and no
  passband carrier recovery — the cyclic extension and the per-carrier
  tracking loop replace all of it. Measured against the V.32bis work
  already in this tree, PEP's physical layer is *easier*, not harder.
- The probe handshake is deterministic and self-synchronising, and the
  two-tone timing trick is a clean, testable primitive worth a unit test
  on its own.
- Waterfilling bit loading is a textbook algorithm with a
  thirty-year literature behind it, and the existing channel simulator
  plus BER harness in `scripts/bench/` is exactly the instrument for
  showing it does what it should.
- The patents expired long ago. "PEP" and "TurboPEP" are Telebit
  trademarks, so the thing is describable as PEP-derived DMT but not as
  a PEP product.

The honest label for the result is "a DMT modem built from the
TrailBlazer patents", not "PEP".

### Interoperating with a real TrailBlazer — no, not from documentation

The gaps above are not details. Constellation labelling, bit ordering
across carriers, a scrambler, and the packet header are each
individually sufficient to make two implementations that are perfectly
correct in isolation fail to exchange a single byte, and the list above
holds ten of them. There is no second implementation to check against, no spec
to appeal to, and no prior RE work to inherit.

That leaves capture-driven reverse engineering against hardware, which
is a real project rather than an impossible one — see below — but it is
a different project from writing a modem, and it must come first.

### Why a real Telebit is an unusually good oracle

If it is attempted, the hardware cooperates to a degree that is close to
unfair. From the T3000/WorldBlazer manual:

- **`S71` / `S73` print the transmit and receive bit allocation per
  carrier, for all 512 frequency points, for the live connection.** The
  modem hands over the demodulator's most important secret in ASCII.
  With the bit map known, a captured epoch has a known number of bits on
  each known carrier, and only the labelling and ordering are left to
  solve — a constrained puzzle rather than an open one.
- `S70` / `S72` give the line rate each way; `S74` gives packet
  statistics (transmitted, retransmitted).
- `S120` forces long packets only, pinning the epoch structure at 7.35
  baud and removing the interactive mode from the captures.
- `S50=255` forces PEP and suppresses fallback, so no V.32 negotiation
  pollutes the recording.

Feeding known plaintext (a long run of a single byte, then a counter,
then a PRBS) through a forced-long-packet PEP connection while logging
`S71` gives a clean, alignable corpus. That is a genuinely tractable
path to the labelling, the bit order, the scrambler and the CRC. The
framing and ARQ layer would follow from watching turnarounds and
induced errors.

The cost is hardware. PEP needs a TrailBlazer Plus, T1000, T1600, T2500
or WorldBlazer — the T3000 alone does *not* have PEP. One unit suffices
if modec is the other end, but two is far better: two Telebits talking
to each other over the HT802's pair of FXS ports produces reference
recordings of a known-good PEP conversation with no unknown side, and
that corpus is the thing everything else gets checked against.

## What PEP was designed against: the late-1980s analog margins

PEP was engineered to a published envelope, and the envelope is still
downloadable. The two documents that matter are contemporaneous with the
TrailBlazer — both are Blue Book, 11/1988.

### CCITT M.1020 (11/1988) — the conditioned analog circuit

[M.1020](https://www.itu.int/rec/T-REC-M.1020-198811-I/en) specifies
"special quality international leased circuits with special bandwidth
conditioning", and its scope paragraph is worth quoting because it names
PEP's design premise exactly: *"circuits meeting the requirements of this
Recommendation are intended for use with modems that do not contain
equalizers."* That is the whole reason PEP has a cyclic guard instead of
an equalizer.

| Parameter | Limit |
|---|---|
| Loss/frequency distortion, rel. 1020 Hz | −1 to +3 dB over 500–2800 Hz; −2 to +6 dB over 300–500 and 2800–3000 Hz |
| **Group-delay distortion**, rel. minimum | **≤ 0.5 ms over 1000–2600 Hz; ≤ 1.5 ms over 600–1000; ≤ 3.0 ms over 500–600 and 2600–2800** |
| **Frequency error** | **≤ ±5 Hz** |
| Phase jitter | ≤ 10° peak-to-peak; up to 15° permitted on "circuits of necessarily complex constitution" |
| Random circuit noise | ≤ −38 dBm0p beyond 10 000 km |
| Signal-to-total-distortion (incl. quantizing) | ≥ 28 dB at −10 dBm0 |
| Impulsive noise | ≤ 18 peaks above −21 dBm0 per 15 min |
| Amplitude hits | ≤ 10 exceeding ±2 dB per 15 min |
| Loss variation with time | ≤ ±4 dB, daily and seasonal |
| Satellite FDM section | ≈ 10 000 pW0p (−50 dBm0p), counted as equivalent to 1000 km of FDM |

Lay PEP's parameters over that and the fit is not a coincidence:

| PEP | M.1020 | Margin |
|---|---|---|
| 8 ms guard interval | 3.0 ms worst-case group-delay distortion at the band edges | 2.7× |
| ±7 Hz offset correction | ±5 Hz frequency error | 1.4× |
| ±1.0°/carrier/epoch phase tracking | 10–15° pk-pk phase jitter | tracks it in ~10 epochs |
| ±0.1 dB/carrier/epoch level tracking | ±4 dB slow loss variation | tracks it |
| Patent's "typical" 4 bits over 75% of carriers ⇒ 11 300 bps | 28 dB signal-to-total-distortion floor | 4 bits/carrier is what 28 dB buys |

The patent's *typical* case is the M.1020 *worst* case. PEP was built to
run at full rate on a circuit that only just meets the Recommendation,
and its headline 22 580 bps only appears on a circuit far better than
spec. A switched dial-up connection was of course worse than an M.1020
leased circuit, which is why the guard is 8 ms and not 3.

### CCITT G.114 (11/1988) — the delay budget

[G.114](https://www.itu.int/rec/T-REC-G.114-198811-I/en) Table 1, the
planning values in force when PEP shipped:

| Medium | One-way contribution |
|---|---|
| Geostationary satellite, 36 000 km | **260 ms**, earth station to earth station |
| 14 000 km altitude satellite | 110 ms |
| Submarine coaxial cable | 6 µs/km (≈ 36 ms transatlantic) |
| Terrestrial coax or radio relay | 4 µs/km |
| Optical fibre | 5 µs/km |
| FDM channel modulator/demodulator | 0.75 ms, or 0.5 ms compandored |
| PCM coder or decoder | 0.3 ms |
| Echo canceller | 1 ms in the send path |
| National extension, analogue | 12 + 0.004 × km ms each end |

with the connection limits: 0–150 ms acceptable, 150–400 ms acceptable
given echo control designed for long-delay circuits, above 400 ms
unacceptable. Q.13 restricted satellite routing so that a connection got
at most one hop. Annex A puts an all-submarine-cable connection at about
170 ms one-way.

So a one-hop satellite call was ≈ 300 ms one-way, ≈ 600 ms round trip —
and PEP ran over it. The delay was never the modulation's problem.

### What actually hurt on those links

Not the 260 ms, and not the modulation:

- **Echo suppressors.** A half-duplex protocol that reverses direction
  every few hundred milliseconds is the worst possible traffic for a
  suppressor on a 600 ms path — it clips the front of every turn. This
  is what `S121` "echo suppressor compensation" is for: PEP mode only,
  costs about 5% of throughput, and is negotiated during PEP
  initialisation so either end can force it on (T2500 manual, p. 5-80).
- **Turnaround economics.** A 136 ms epoch against a 600 ms round trip
  means each reversal costs about 4.4 epochs of dead air. PEP's answer
  was structural — the I…N epoch allocation lets a sender hold the line
  for up to N epochs — and, more importantly, the spoofing: UUCP-g,
  Kermit and XMODEM were terminated *in the modem* so the file-transfer
  protocol's tiny window never had to cross the satellite at all. On a
  long-delay link that spoofing was the entire product.
- **Clipping and companding.** TASI on analog submarine cable and DSI/DCME
  on satellite discard roughly 1% of speech — harmless for voice, fatal
  for data — so data was kept off interpolated channels. PEP's `J6 S36`
  protective tone, prepended to each packet, is the generic answer to
  clipping on circuits that do it anyway.

### Why VoIP is a different hazard, and where it is not

Latency, jitter and clock slip are three different things, and only the
third is a problem for PEP.

An analog satellite circuit is a **sample-synchronous** pipe: 260 ms
long, but FDM translation shifts a signal in frequency without ever
adding or removing a sample. There is exactly one clock. A jitter buffer
does not reproduce that. It converts arrival variance into constant
delay, which PEP would not notice — until it under- or overruns, at
which point it inserts or deletes samples, and packet-loss concealment
fabricates them. That is a clock slip, and the analog network never
produced one.

The cost of a single slip is easy to put a number on. 125 µs at 3000 Hz
is 135° of phase rotation on that carrier, scaling linearly with
frequency across the band. The tracking loop moves ±1.0° per carrier per
epoch. One slipped sample is therefore ~135 epochs — 18 seconds — of
correction, if the loop converges at all rather than losing the timing
reference. Two free-running 8 kHz oscillators differing by 10 ppm slip a
sample every 12.5 s; at 50 ppm, every 2.5 s, or once every 18 epochs.

But this only bites where there are **two clock domains**:

- **HT802 FXS-to-FXS, both ports on one ATA: one crystal, zero relative
  drift, LAN-scale jitter that any fixed buffer absorbs.** This is a
  legitimate PEP bench and the caution does not apply to it. Use a fixed
  jitter buffer, disable PLC, and the path is effectively sample-clean.
- A voip.ms trunk, or any two endpoints with independent oscillators, is
  where slip is structural rather than incidental. Treat it as an
  experiment, not a baseline.

G.711 itself is *not* the new hazard, and it is worth being clear about
that: by 1988 the domestic network was already µ-law T-carrier, M.1020
budgets quantizing distortion explicitly in QDUs, and the 28 dB
signal-to-total-distortion floor above already includes it. PEP lived on
companded PCM trunks from the day it shipped. What µ-law does demand is
headroom for PEP's crest factor — a few hundred random-phase carriers
run about 10–11 dB peak-to-average — so level setting matters, but that
was equally true on a 1988 T1.

## Verdict

Feasible, with the scope stated honestly:

- A patent-faithful 512-carrier DMT modem with waterfilling bit loading,
  probe-based training, and the two-tone timing recovery is a
  well-specified, self-contained piece of work that this codebase is
  already equipped to test. Recommend it on its merits — it is a cleaner
  physical layer than V.32bis and it would exercise the bench harness in
  a new dimension.
- A PEP implementation that a TrailBlazer will answer is a hardware
  reverse-engineering project gated on acquiring a PEP-capable Telebit.
  It is tractable — `S71`/`S73` make it much more tractable than it has
  any right to be — but it is not a documentation problem, and no amount
  of further searching will close the gaps listed above.
- The bench for either is the HT802's two FXS ports, which share one
  clock. That is a sample-synchronous path and PEP will run on it. A
  voip.ms trunk is a separate experiment with its own failure mode.

Local copies of the four patent PDFs and the four manuals, with text
extractions, are in this session's scratchpad; the URLs above are the
durable references.
