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

## What the call actually exposed, and the fix

The failure had nothing to do with V.32 or V.8, and the recording is
what showed that. Replaying it through the whole modem
(`scripts/diag/replay.hs`) with V.32 and V.8 removed fails in exactly
the same place, and the receiver is not the problem either: through the
V.22 phase it is locked at an EVM of 0.0002, decoding the far end
perfectly.

The fault was ours, and it is a plain reading of 6.3.1.2. The answering
modem sends scrambled binary 1 only once it has detected the *calling*
modem's unscrambled binary 1. modec sent its unscrambled ones for a
fixed 406 ms and then moved on to scrambled ones -- and at 1200 bit/s
skipped them altogether, going straight from the 456 ms silence to
scrambled ones. On a trunk with 150 ms each way, a 406 ms burst is over
before the far end has finished qualifying it. This far end sat on its
own unscrambled ones for three full seconds waiting to be answered, gave
up, and offered V.21 instead -- which is exactly what the tone list
above shows.

It survived so long because modec's *answerer* had the mirror fault: it
advanced on the caller's scrambled ones rather than its unscrambled
ones, so the two cancelled and modec talked to itself perfectly. Both
sides are fixed, and `handshake simulation / the calling modem holds
unscrambled binary 1 until it is answered` drives the calling side
against a synthetic answerer that behaves the way this BBS does. Without
the fix it reports "1200: never sent unscrambled binary 1".

Replayed now, the caller holds unscrambled binary 1 from 14.08 s to
17.08 s, overlapping almost the whole of the far end's own 13.26-16.34 s
window. What that cannot show is the connection completing: in replay
our transmissions go nowhere, because the far end's audio is fixed. The
recording proves the diagnosis and the fix addresses it; only another
call will prove the connection.

## live-v32-9600-heard.wav — the second call, and V.32 at 9600

Same number, dialled the same way, and this time the far end had V.8 and
V.32 both: `V.8 far end offers: data, V.32bis/V.32, V.22bis/V.22, V.21`
at 13.07 s, and `CONNECT V32 9600 bit/s ... trellis coded` at 41.51 s.
The first live V.32 connection, and it held for the ninety seconds the
call was given.

It also delivered nothing, and the 41 s is why. Figure 4 should be done
by 24. The bit history was capped at 96 bits with the detectors looking
at 64 of them, while one 20 ms block carries 96 bits at 4800 bit/s and
192 at 9600 -- so every block put in more than was ever looked at. A
rate signal repeats until it is answered and so was always caught in the
end; E is sent exactly once, and this call put it in the gap. The modem
sat in B1 for seventeen seconds, took a noise coincidence for E, and
arrived in data mode long after the far end had finished its banner.

Replayed against the fix it reaches data at 24.40 s and reads it:

    https://2600.network - Patton 3120 #1
    uSerNaME:

The rubbish after that is the far end reacting to a modem that was still
sending E at it, and a fixed recording cannot show what it would have
done instead. That still wants another call.

Note what this call does *not* verify. It never reached the V.22 phase
the first call failed in -- V.8 picked V.32 immediately -- so the
unscrambled binary 1 fix above is still unconfirmed against a real
modem. This number answers with different hardware on different calls;
it gave V.23 at 1200/75 an hour earlier.
