# Testing MNP against a real modem

Ten calls to real boards established MNP three times and left several
questions that calls are a poor way to answer: they cost money, they tie up
someone else's line, and every one of them changes the line conditions
underneath the thing being measured. A modem on the bench answers the same
questions repeatably and for free.

This is what is known, what is not, and the order to test it in.

## Where it stands

**Working, and confirmed against real hardware.** Three boards negotiated
MNP with us and the recordings decode frame by frame: A-Net Online at class
2, Basement BBS and C64 Pub at class 4 with synchronous framing. Basement's
information fields reassemble into its banner, byte-clean, through the
switch to bit-oriented framing. Those two exchanges are now fixtures in
`test/fixtures/mnp/`, so the decoder is pinned against hardware rather than
against our own encoder.

**Confirmed by that hardware, and worth not forgetting.** Neither A-Net nor
C64 Pub sends the constant parameter 2 the Recommendation prints. They send
`7,1,247,0,0,1` and `7,7,225,0,0,17`. Validating that field would refuse
both links.

**Unreliable, cause not yet established.** Across two batches the same five
boards answered a link request on one call and not the other, roughly a
coin toss, and anti-correlated between the batches rather than favouring
either class. Probing was widened from two attempts over six seconds to six
over fifteen on the theory that we were asking in too narrow a window --
V.42's detection phase is abandoned 750 ms into the data phase, so arriving
late finds a modem that has already settled for no error correction, while
a far end whose error-control entity is not ready yet needs asking again.
That change has not been measured against anything.

**Fixed since those calls, and untested on the line.** A far end that had
opened its data phase was being read as silence: only the frames the
establishment exchange was waiting for counted as evidence of a protocol,
so information frames arriving during establishment did not stop the
fall-through. That is what corrupted A-Net's banner -- in transparent mode
the frame headers and check sequences go to the terminal as characters.

**Not the protocol's fault, and still open.** The five boards called after
that fix all connected at 1200 bit/s and returned `0x33`/`0x77` repeating,
which is the V.22bis S1 pattern: the far end never left rate negotiation
and never entered a data phase at all. There is nothing for MNP to run
over. That is a handshake problem, and MNP only made it audible.

## What needs no modem at all

Run these first; they cost nothing and they fail faster than hardware.

```
cabal test                       # 161 tests, including the two recorded exchanges
scripts/smoke-loopback.sh        # two modems through FIFOs
```

To add `--mnp` to the loopback, edit the two `modec modem` lines in
`scripts/smoke-loopback.sh`. Both ends negotiate class 4 with synchronous
framing and the text passes both ways.

## The bench ladder

Each step isolates one thing. Do them in order: a failure at step *n* makes
everything after it meaningless.

Throughout, capture the call so a failure can be read back rather than
guessed at:

```
modec modem --originate --mnp --record-rx /tmp/rx.wav --trace ...
cabal exec -- ghc -O2 -package modec -package bytestring \
  scripts/mnp-decode.hs -o /tmp/mnp-decode
/tmp/mnp-decode /tmp/rx.wav
```

The decoder reads both framings and reassembles whatever the information
fields carried, so it says what the far end actually sent even when the
live modem made nothing of it.

### The modem's own settings

Error-control commands differ by make, and the USR family differs from the
Rockwell-style convention that most documentation assumes. Check `ATI` and
the manual before trusting any of these; on USR modems `&K` is compression
where elsewhere it is flow control, which is an easy way to run a whole
session against the wrong setting.

- Rockwell-style: `AT\N0` no error control, `\N2` MNP required, `\N3`
  auto-reliable, `\N4` LAPM required, `\N5` MNP required. `AT%C0` disables
  compression.
- USR-style: `AT&M0` normal, `&M4` auto-reliable, `&M5` reliable required.
  `AT&K0` disables compression. `ATI7` prints the configuration.

Turn compression off for every step below. MNP 5 is not implemented, and a
far end that insists on it will fail for a reason that has nothing to do
with what is being measured.

### 1. No error control at the far end

Set the modem to no error control. Connect with `--mnp`.

*Proves:* the fall-through is silent and does not corrupt. Expect no link
request answered, no disconnect sent, and the far end's text intact.

*If it fails:* the fall-through is either sending a disconnect it should
not, or handing frame octets to the terminal. Compare what the terminal
received against what `mnp-decode` says was on the line.

### 2. MNP required, LAPM disabled

*Proves:* the core interworking. Expect `MNP class N` in the log and
`PROTOCOL: MNP CLASS N` at the terminal.

*If it fails:* decode the recording. Either the far end sent no link
request at all -- in which case it is a timing or detection question, go to
step 6 -- or it sent one we did not act on, which is a decoder bug and the
fixture tests should be extended with it.

### 3. LAPM required

*Proves:* we behave when the far end wants V.42 and will not fall back.
Expect our link requests ignored, a clean fall-through, and readable text.

*If it fails with garbage:* the fall-through is mis-firing, which is what
step 1 also tests. If the far end instead drops the call, our link requests
are being read as something harmful and that is worth knowing before any
more real calls.

### 4. Auto-reliable

The realistic default, and what a board is most likely to be set to.

*Proves:* the far end chooses MNP when we offer nothing else. This is the
setting that most resembles the ten calls already made.

### 5. Rate

Repeat step 2 forced to 1200 bit/s (V.22), then to 2400 (V.22bis).

*Proves:* the synchronous path at both rates. The transmitter had a bug
where 2400 bit/s synchronous framing sent half its bits on the 1200 bit/s
constellation -- invisible at 1200, fatal at 2400, and only found by
driving real binaries. The suite now covers it, but this is the cheap
confirmation on hardware.

### 6. How hard to try

The open question. Repeat step 4 ten times at each setting and count how
many establish:

```
--mnp-probes 1  --mnp-probe-interval 2.5
--mnp-probes 6  --mnp-probe-interval 2.5     # the current default
--mnp-probes 6  --mnp-probe-interval 0.5     # early and often
--mnp-probes 3  --mnp-probe-interval 8       # closer to Microcom's own timer
```

*Proves, or refutes:* whether the coin toss seen on real calls is a probing
window problem. Ten runs per setting is enough to tell 100 % from 50 %; it
is not enough to tell 90 % from 100 %, so do not read small differences.

*If every setting establishes every time:* the unreliability is not
probing, and the next suspect is the line -- the boards that failed were
also the ones that connected at 1200 with a poor receiver lock.

### 7. Class by class

Repeat step 4 with `--mnp-class 2`, `3`, `4`.

*Proves:* which layer breaks if one does. Class 2 is start-stop framing
only; 3 adds the switch to synchronous framing; 4 adds the shorter headers
and adaptive frame sizing. A failure that appears only at 3 is the switch;
only at 4, the optimization.

### 8. Both directions

Have the modem dial modec, so modec answers.

*Proves:* the roles are read from the established link rather than from the
configured role. The calling side sends the first link request, and V.8bis
can reverse who that is.

### 9. Data integrity under impairment

Send a known pattern both ways and compare byte for byte -- a file through
the telnet side is easiest. Then attenuate the audio path, or add noise,
until the raw link is damaging bytes, and confirm the delivered stream is
still exact.

*Proves:* the thing the whole exercise is for. Without impairment this only
demonstrates that a clean link stays clean.

## What to record

For each run: the modem's settings, `--mnp*` arguments, the log, and the
`--record-rx` WAV. The recordings are the only artefact that survives a
misdiagnosis -- both times this session that a live log said one thing and
the recording said another, the recording was right.
