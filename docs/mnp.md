# MNP error correction

Without error correction a dropout costs you the characters it lands on and
nothing tells you it happened. `--mnp` puts a protocol between the terminal
and the data pump: it cuts what you type into numbered frames, checks each
one, and asks again for whatever the line damaged.

modec implements classes 2, 3 and 4, from **ITU-T V.42 (10/96), Annex A**.
That annex is MNP with the trade name filed off — the frame type codes, the
constant parameter `1, 6, 1, 0, 0, 0, 0, 255` and the retransmission limit
of 12 all match Microcom's own public-domain release of 1987, which is how
the identification is confirmed.

**Get the 10/96 edition.** The 03/2002 revision deleted Annex A, and its
summary says so. Downloading the current V.42 from itu.int gets a document
with none of this in it.

## Using it

```
--mnp                   offer everything: classes 2 to 4
--mnp-class 2           start-stop framing only
--mnp-class 3           adds synchronous framing
--mnp-class 4           adds the data phase optimization and adaptive sizing
--mnp-round-trip S      round trip the retransmission timer allows for (0.5 s)
```

It is off unless asked for. When it comes up the log names what was agreed,
and a terminal in Hayes mode is told on its own line after `CONNECT`, the
way the modems that spoke this protocol told it:

```
CONNECT 2400
PROTOCOL: MNP CLASS 4
```

## What the classes are

| Class | What it adds |
|---|---|
| 2 | Start-stop framing over the ordinary 8N1 characters the pump already carries: `SYN DLE STX … DLE ETX` and a CRC-16/ARC. Full duplex, go-back-N, a credit window. |
| 3 | Synchronous framing: the start and stop bits between the two modems go away and the frames become ISO 3309 HDLC. Worth a fifth of the line. V.22 family only — the FSK links offer class 2 and the negotiated minimum settles it. |
| 4 | The data phase optimization, which shortens the LT and LA headers from 5 and 8 octets to 3 and 4; information fields up to 256 octets; and adaptive packet assembly, which sizes the frames to the line. |

Class 5 compression is **not** implemented and is not planned. Its
adaptive-frequency token table is not published anywhere I could find, so
nothing written from the public descriptions would interoperate.

## What it costs on the line

Measured, for one frame carrying an information field of the given size:

| Information field | Class 2 | Class 3 | |
|---|---|---|---|
| 256 octets | 2670 bits | 2104 bits | 21 % less |
| 16 octets | 270 bits | 185 bits | 31 % less |

A large frame saves the fifth that dropping the start and stop bits is
worth. A small one saves more, because HDLC's two flags and check sequence
are cheaper than mode 2's four octets of lead-in and four of trailer.

## Establishment, and what happens when the far end has none

The exchange is three messages and is always start-stop framed, whatever
gets negotiated; the negotiated framing starts afterwards.

```
initiator  ── LR ──▶  responder     link request: what I have
initiator  ◀── LR ──  responder     the reply, which is what both then obey
initiator  ── LA ──▶  responder     and the data phase is open
```

There is no detection pattern to send first. The answering side simply
listens, and the calling side's repeated link request is both the request
and the probe. A V.42 modem will hear it: V.42 §7.2.1.3 makes an incoming
alternative-procedure frame one of the ways its own detection phase ends.

If nothing answers, A.7.2.2 is explicit that **no disconnect is sent** and
the connection carries on unprotected. Everything heard while waiting is
handed to the terminal rather than dropped — on a call to a board with no
error correction, that was the banner.

A far end with nothing to say for itself is recognised by what it sends
looking like text. Counting octets alone would be wrong: on a line bad
enough that no frame survives, a far end that *does* speak the protocol
produces nothing but wreckage, and reading that as "no protocol over there"
abandons error correction exactly where it earns its place.

## Numbers worth knowing

* Sequence variables start at **1**, not 0.
* `N(R)` names the **last frame correctly received**, not the next
  expected, which is why the acknowledgement closing establishment carries
  zero.
* An acknowledgement **repeating** the previous `N(R)` is a negative
  acknowledgement, and arrives long before the timer would.
* The **first** repeat of the frame just taken draws no acknowledgement at
  all. Answering it turns one duplicate into an exchange that never settles.
* N400, the retransmission limit, is 12. Past that the link is declared
  dead, which on a line losing half its blocks is the right answer.

## Sources

* [ITU-T V.42 (10/96)](https://www.itu.int/rec/dologin_pub.asp?lang=e&id=T-REC-V.42-199610-S!!PDF-E&type=items) — the edition with Annex A
* [jduerstock/mnpc12](https://github.com/jduerstock/mnpc12) — Microcom's
  public-domain class 1–2 source, 1987: timers, window sizes, retry counts
* spandsp's `v42.c` is LAPM only and has none of this
