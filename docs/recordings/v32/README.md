# V.32 recordings: a loopback and a live call

Three recordings, and what reading them back says about where V.32
stands. All are 8 kHz 16-bit mono, as received (`--record-rx`), so each
file is what that modem's own receiver had to work with.

Read them with the tool that shares the start-up's detectors:

    cabal run modec -- v32trace docs/recordings/v32/loopback-caller-heard.wav
    cabal run modec -- v32trace --answer docs/recordings/v32/loopback-answer-heard.wav
    cabal run modec -- detect   docs/recordings/v32/live-bbs-heard.wav

`v32trace` reports what `Modec.V32Start` can see -- the answer tone, the
phase reversals in AA and AC, the conditioning signal, the rate signals
and signal E -- run over a file rather than a live line. `--answer` says
which end we were, because that decides which scrambler the rate signals
are read with.

## loopback-caller-heard.wav, loopback-answer-heard.wav

Two modems over a pair of pipes, `--mode v32`, connecting at 9600 bit/s
trellis coded. This is the controlled reference: no echo, no noise, no
jitter, and both ends are this implementation.

The calling modem's file reads:

    3.100  tone at 600 Hz / 3000 Hz     the answerer's alternating AC
    3.345  phase reversal at 600, 3000  AC -> CA: the answerer heard our AA
    3.648  phase reversal at 600, 3000  CA -> AC: it heard our AA -> CC
    3.900  conditioning signal (S)      segment 1, and our own timing reference
    4.620  rate signal 4800 9600 tcm (V.32bis)
    6.860  signal E 9600 tcm            the rate both ends settled on

Every step of Figure 4 is there, in order, with the two reversals 303 ms
apart -- that gap is NT, the round trip this end measures for itself and
hands to the echo canceller. The answering modem's file is the mirror
image: our AA at 3.220, its two reversals, our conditioning signal at
4.740, our rate signal, and E.

Two things worth noticing. The rate signal reads *4800 9600 tcm
(V.32bis)*: bits B4 and B8 are set, which is how a V.32bis modem
announces itself under Table 5/V.32 bis Note 1, while B9, B10 and B12
are clear because 7200, 12000 and 14400 are not in the default offer.
And the reversals continue all through the data phase -- a 32-point
constellation crosses 1800 Hz phase constantly, so the tracker keeps
firing. That is not a fault; it is what a reversal detector does once
there is data rather than a tone to look at, and the state machine has
stopped asking it by then.

## live-bbs-heard.wav

One call over voip.ms to a BBS on +1 564 244 2600, dialled
`--mode v32,v22bis,v22,v21 --v8`. It did not connect, and the recording
says exactly why. `detect` gives the tone sequence:

     6.38 -  6.58 s   400 Hz     V.8bis CRe from us, unanswered
     9.16 - 13.28 s   2100 Hz    V.25 answer tone, in segments
    13.26 - 16.34 s   2250 Hz    V.22 unscrambled binary 1
    17.06 - 18.66 s   1650 Hz    V.21 channel 2 mark

That is a V.22bis answerer working down its ladder: answer tone, then
V.22's binary 1, then V.21's mark. What is *not* there is any energy at
600 and 3000 Hz -- the alternating AC that a V.32 answering modem sends
after its answer tone, and the only thing a V.32 calling modem is
listening for. `v32trace` finds nothing before 16 s and then only a
single 3000 Hz run, which is the 2250 Hz V.22 tone leaking into a
detector aimed 750 Hz away.

So the far end is a 2400 bit/s modem that does not offer V.32, and no
amount of work at this end will make a V.32 call to it succeed. The
useful conclusion is about the population rather than the code: the
BBSes reachable on this trunk answer V.22bis, and a V.32 connection
needs a far end that speaks it. The place to test V.32 against real
hardware is the USR modem on the bench, not a dialled BBS.

The call also showed something at this end that is *not* fixed. Dialled
`--mode v32,v22bis,v22,v21 --v8`, against a far end offering V.22 and
V.21 and no V.8 at all, the caller did not fall back: it ran to
`connection failed: timeout` at 46 s rather than taking the 2250 Hz it
had been hearing since 13 s. A V.22-only dial to the same number
connects, so the automode ladder is being held up by something in the
V.8 or V.32 path rather than by the line. That is the next thing to
chase, and this recording is the case to chase it with.
