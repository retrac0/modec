# Connecting a real modem (e.g. a USR Courier/Sportster) to a sound card

A dial-up modem does not talk to "audio"; it talks to a 2-wire telephone
loop. To make it go off-hook, dial and train, the loop must supply:

1. **DC loop current**: the modem's DAA (data access arrangement) draws
   roughly 20–40 mA when off-hook and uses that to detect that a line is
   present. A sound card supplies none.
2. **AC audio** on the same pair, at telephone levels: about −9 dBm
   transmit (≈0.27 Vrms into 600 Ω) and anything from −10 to −40 dBm receive.
3. Optionally **dial tone** (skip with `ATX3` so the modem blind dials) and
   **ringing** (skip by answering with `ATA`; the modem never needs to see a
   ring if you tell it to answer).

There is never any need to connect the sound card to a live PSTN line, and
you should not: ringing is 90 Vrms and the line carries 48 V DC.

## Option A: passive line simulator plus transformer (the classic circuit)

```
      +12 V DC (wall adapter or 9 V battery is fine)
        |
       [R1]  330–470 Ω, 1 W
        |
   +----+----------------------------+--------------------+
   |                                 |                    |
  TIP                             C1 1 µF–2.2 µF        (second modem, if
   |                             film, non-polar          you want a modem
  [MODEM DAA]                        |                    to modem link
   |                            T1 600:600 Ω              instead: connect
  RING                          telephone / audio         its TIP/RING here)
   |                            isolation transformer
   +----+---------------------------+|
        |                            |
       [R2]  330–470 Ω, 1 W          |     secondary side (sound card)
        |                            |
       0 V (GND of the 12 V supply)  +----[R3 10 kΩ]----+---- LINE IN (tip)
                                     |                  |
                                     |                 [R4 1 kΩ]
                                     |                  |
                                     +------------------+---- LINE IN (sleeve/GND)
                                     |
                                     +----[R5 600 Ω]--------- LINE OUT / headphone (tip)
                                     |
                                     +----------------------- LINE OUT (sleeve/GND)
```

- R1 + R2 (≈ 660–940 Ω total) plus the modem's own ~200 Ω off-hook
  resistance sets the loop current: 12 V / ~1 kΩ ≈ 12–25 mA, enough for
  every DAA I know of. If the modem refuses to go off-hook (`NO DIALTONE`
  even with `ATX3`, or it drops instantly), raise the supply to 24 V.
- C1 blocks the DC so it never reaches the transformer or the sound card.
  Use a film capacitor rated ≥ 50 V.
- T1 gives galvanic isolation and the 600 Ω AC termination the modem
  expects. Any 600:600 telephone coupling transformer works (Tamura
  TTC-108, Xicon 42TL016/42TL018, Bourns LM-NP-1001, or a transformer
  pulled from an old modem or answering machine).
- R3/R4 drop the ~0.3 Vrms line level to something a mic/line input is
  happy with; adjust or omit R4 for a proper line-in. R5 keeps the sound
  card output from shorting the line and sets the injected level; the
  sound card's volume control does the rest. Aim for about −10 dBm on the
  line, which is 0.25 Vrms across the transformer.
- The sound card hears its own transmit signal (there is no hybrid). That
  is harmless for Bell 103, V.21 and V.22/V.22bis because each direction
  uses its own frequency band and the receiver filters the other band
  out. It becomes a problem only for echo-cancelling modulations (V.32 and
  up), which is not the goal here.
- An external USR modem's DAA is already isolated from its RS-232 ground,
  so a shared computer ground is not a hazard. With an internal modem the
  DAA is likewise isolated by design; the transformer above isolates the
  sound card anyway.

Procedure: power the loop, `ATX3D` (or `ATX3DT123`) on the modem, and it
goes off-hook and sits in originate mode waiting for answer tone. Play
2225 Hz (Bell) or 2100 Hz (V.25) from the sound card for ~3 s and it will
start the handshake. To have the modem answer instead, send `ATA`.
For Bell 103 specifically force it with `AT&N1` (USR) or `AT+MS=B103`.

## Option B: a second modem as the "line"

The same DC feed (12 V through 2 × 470 Ω) between two modems lets them
talk to each other with no sound card at all; hang the transformer across
the pair to eavesdrop with the sound card. That is the cheapest way to
get real-hardware handshake recordings for the test fixtures.

## Option C: an ATA (analog telephone adapter)

A Grandstream HT801/HT802, Cisco SPA112, Obihai OBi200 or similar gives a
proper telephone line (loop current, dial tone, ringing, caller ID) and
turns it into SIP/RTP with G.711. That is exactly the SIP leg this project
wants eventually, so it is the most future-proof option: the modem plugs
into the ATA, the ATA registers to Asterisk/FreeSWITCH or directly to a
SIP user agent on the PC, and the audio arrives as 8 kHz G.711. Set the
codec to PCMU/PCMA, disable silence suppression, VAD, comfort noise and
echo cancellation on the ATA, and use a fixed jitter buffer.

## Option D: a commercial line simulator

Viking DLE-200B and Teltone TLS-3/TLS-5 give two RJ11 ports with battery,
dial tone and ringing. Useful with Option A's transformer for monitoring.

## Levels and safety summary

- Nothing in Option A exceeds 24 V DC; the only genuinely dangerous
  source is a real phone line, which is not used.
- Keep the DC out of the sound card (C1 and T1 both do this).
- Keep the sound card output from driving the transformer directly at full
  swing; a 600 Ω series resistor and the mixer volume are enough.
- If the sound card input is a mic input with plug-in power (a small DC
  bias on the tip), the transformer secondary shorts it to ground through
  R4; add a 10 µF capacitor in series with R3 if that bothers the card.
