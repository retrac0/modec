# Recorded calls, as fixtures

Twelve calls placed over the voip.ms trunk in September 2026, trimmed to
the window that carries the answer, and replayed through the whole modem
by `Corpus.liveTests`. Every one of them had a real modem at the far end.

Each fixture is three files with one name:

| file | what it is |
|---|---|
| `NAME.wav` | the audio as received, 8 kHz 16-bit mono, trimmed |
| `NAME.txt` | the reference: what it decoded to on the day it was minted |
| `NAME.call` | how to replay it, and what is known about it |

## The spec

    source:    recordings/20260907T155453-+15642442600.wav
    seconds:   30
    role:      originate
    modes:     v32,v22bis,v22,v21
    v8:        yes
    mnp:       4
    connect:   V32 9600
    expect:    https://2600.network - Patton 3120 #1
    tolerance: 0

`connect:` is the standard and bit rate the call must reach, or `none`
for a recording that must not connect at all. `expect:` lines -- there
may be several -- are the hand-verified truth: text the far end really
sent, read off the service's own documentation or off the board by eye.
Those are what must never break.

`tolerance:` is weaker and does different work. The reference is not
truth, it is a record of one decode, junk included; the tolerance says
how many bytes of edit distance the decode may drift from it. At 0 it
pins the decode exactly, which is what every fixture here does today.

A deliberate improvement that moves a reference is accepted by minting
the fixture again, from the same source and window the spec names:

    cabal run modec -- replay --mint NAME --mode M --seconds N \
      [--v8] [--mnp] recordings/SOURCE.wav

## Why the whole modem

Pointing a receiver at the top of a recording does not work, and it fails
in a way that looks like success: it acquires on ringback and the answer
tone, locks to nothing, and returns a constant that the descrambler turns
into a page of `U` or `w` characters. A live modem never does that -- the
handshake starts the data receiver at the right instant, at the right
rate, in the right channel. So these run the real modem over the
recording, which reproduces the live decode exactly, banner for banner.

Our transmissions go nowhere: the far end's audio is fixed. That is sound
here because a live modec already drew these responses out. What it
cannot do is ask how the far end would have answered something else.

## The oracle

Five of the twelve are the same far end. 2600.network on +1 564 244 2600
is a Patton 3120 access server that answers every call with the same two
lines, and six of modec's modulations have connected to it. Five are
here -- Bell 103 is not, because that call ran out before the banner
finished -- and each carries the identical fifty-one bytes at 300, 1200,
1200/75, 2400 and 9600 bit/s. A fixed string from a machine that never
varies is a better oracle than any BBS banner, because there is nothing
to argue about when it comes back wrong.

The rest are boards, for the variety the oracle cannot give: a
Synchronet banner and a far end that hangs up mid-sentence, a 2400 bit/s
link negotiated through V.8, ASCII art at 1200, a FidoNet mailer's
checksummed EMSI, ANSI escapes at 300 bit/s, a WILDCAT! registration
line, and one call that must not connect at all.

None of this reaches the network. The audio is on disk and the tests
read it; see the note on the test-suite stanza in `modec.cabal`.
