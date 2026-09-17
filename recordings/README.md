# Call recordings

Received audio (`--record-rx`) from real calls, named by the time they
were placed, the BBS, and the configuration used. All are 8 kHz 16-bit
mono, the G.711 audio as it arrived from the voip.ms trunk. Read them
back with `modec detect` and `modec probe`.

Only a handful of these are tracked in git; a sweep is fifty megabytes,
so `recordings/*.wav` is ignored and the files live on disk only.

## The BBSes

Numbers come from the dial-up list at telnetbbsguide.com. Twenty-five
were called; twenty answered.

## What connects

One call per BBS per configuration, ringing for up to 32 s and then
holding for a fixed budget measured from the far end's **answer tone**,
not from the trunk's early-media ringback. A ringing line is not an
occupied one, and the trunk answers our INVITE immediately, so timing
the budget from SIP establishment would have spent it all on ringback.

| Configuration | Calls | Connected | Carried readable data |
|---|---|---|---|
| `--modes v22bis` | 3 | 3 | 2 (one at 2400, one at 1200) |
| `--modes v22bis --v8` | 3 | 2 | 2 (one at 2400 via V.8) |
| `--modes v22` | 2 | 2 | 2 (both 1200) |
| `--modes v21` / `--v8` | 4 | 0 | 0 |
| `--modes bell103` | 2 | 1 | 1 (300 bit/s, real ANSI) |
| `--modes bell212a` | 12 | 0 | 0 |
| default (all modes) | 4 | 3 | 0 |

Banners that came back intact:

- **A-Net Online**, V.22bis 2400: `Synchronet External POTS Support v1.32`
- **Basement BBS**, V.22bis 2400 negotiated over V.8: `Synchronet External POTS Support v1.30-Win32`
- **Kludge BBS**, V.22 1200: a full ASCII-art banner
- **C64 Pub**, V.22 1200: a terminal-type menu, and it answered our bare returns
- **Sursum Corda**, V.22 1200: `**EMSI_REQA77E` -- a FidoNet mailer, not a BBS prompt
- **Empire of the Dragon**, Bell 103 at 300 bit/s: `Checking for ANSI...` with escape sequences intact

## Connected but no data

Five calls reported CONNECT at 1200 and then delivered bytes that were
identical, character for character, at unrelated BBSes. Two independent
systems do not send the same forty characters: that is this end's
descrambler running on an idle carrier. The framer arms and starts
delivering before the far end has anything to say. The signatures are
`hbljrY>jS8vrTEMe` and `b|Qw&'Vtd]b%bhjT`; anything containing them is
our own noise, not a banner.

## Bell 212A

Twelve attempts across twelve BBSes, none connected.

The reason is visible in the recordings once `modec detect` stopped
mislabelling the tone. 2250 Hz -- the unscrambled binary 1 an answering
V.22 modem sends -- was absent from the tone bank, so it was reported as
the nearest bin, 2225 Hz, which is the Bell answer tone. That made it
look as though nearly every BBS was offering Bell modes. It was not:

```
 4.820 -   9.860 s  2100 Hz     ANSam: V.8 capable
 9.880 -  12.960 s  2250 Hz     V.22 unscrambled binary 1
```

There is no Bell answer tone anywhere in that ladder. These are
V.34-class modems whose fallback goes ITU, and some of them offer a Bell
103 rung below it -- Empire of the Dragon connected at 300 bit/s and sent
real text -- but none of the twelve offered Bell 212A.

## V.8 capability survey

`--v8-offer-all` advertises every modulation in Table 4 so the joint menu
comes back in full rather than as the intersection with a two-item offer.
Eleven of the twenty that answered replied with a menu:

| Modulation | BBSes |
|---|---|
| V.34 duplex | 11 |
| V.32bis/V.32 | 11 |
| V.22bis/V.22 | 11 |
| V.23 duplex | 7 |
| V.21 | 7 |
| V.17 | 6 |
| V.34 half-duplex, V.29, V.27ter, V.26ter, V.26bis, V.23 half-duplex | 1 (Phoenix BBS) |

Phoenix BBS answers with twelve modulations including the fax carriers,
which is a fax machine's menu rather than a BBS's.

Every one of the eleven has V.34, and none has anything modec can run
except V.22bis/V.22 and, for seven of them, V.21. That is the whole
picture: these are modern modems being asked to speak 1980s protocols,
and they do, down to 1200 and occasionally 300.

## What answered: the whole corpus, classified

`modec classify` reads every recording afresh and says what was on the
other end, from the audio alone. It uses:

- the call-progress detector
- ANSam
- a steady-carrier check
- T.30 frames on V.21 channel 2
- a voice detector

Each verdict comes with the evidence it rests on and when that evidence
was heard.

```
cabal run modec -- classify --truth --by-number \
  --candidates bbslist.txt --candidates dialup-candidates.txt
```

Over the 309 calls to real numbers (bench and loopback calls left out;
about 40 s on four cores):

| Heard | Calls |
|---|---|
| modem | 223 |
| no audio (every sample zero: no media arrived) | 30 |
| congestion | 12 |
| no answer (ringing, nothing after) | 12 |
| unclassified | 12 |
| silence | 10 |
| voice | 8 |
| fax | 2 |

Against the 136 calls whose logged outcome implies a class, 109 agree.

- **Modem and congestion agree completely.** All 69 logged connections
  (or lost carriers) are heard as modems, and all 12 congestion calls as
  congestion.
- **22 calls logged "no answer" were answered by a modem.** Each has an
  answer tone and ANSam 7 to 15 s in. The modem gave up or failed to
  recognise the answer at the time; the line had a modem on it. NIST
  ACTS is one of them.
- **The congestion calls are a recorded announcement first.** 18 s of
  speech, then 480+620 Hz at a quarter second on and off. The classifier
  reports both and lets the cadence decide.
- **Two boards answered as fax machines.** Sursum Corda and Last
  Telegraph Office, on their Bell 212A attempts, fell back to T.30 after
  their data modes found nothing to talk to. They sent CSI and DIS every
  five seconds. Sursum Corda's CSI is its own number, `256 895 4786`.
- **The first sweep's 300 bit/s pass reached an announcement, not the
  boards.** Every board on that pass played a spoken network announcement
  ending in a short tone. That points at the pass's dial string, not the
  boards. These are the `voice` verdicts on BBS numbers.
- **One private number (+19208406311) rang five times and was answered by
  a voice.** The recording is not tracked.
- **Wizard's Rainbow's 2400 attempt is not line audio.** It is a pair of
  tones at 1333 and 2667 Hz, 0.2 dB below full scale. No G.711 trunk
  delivers that, so it is left unclassified and flagged as full scale.
- **Unclassified calls are mostly fragments.** A second of a previous
  call's carrier, a lone ring or a single word at the head of a
  recording.

Five of these calls, cut short, are fixtures in `test/fixtures/calls`
with the class each must keep.
