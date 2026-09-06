# Mode negotiation

modec supports five modes. Which ones it will negotiate is configured with
`--modes`, a comma-separated list in order of preference (best first):

```
--modes v22bis,v22,bell212a,v21,bell103      # the default
--modes bell212a,bell103                     # a North American 1200/300 modem
--modes v22                                  # V.22 only: 1200 bit/s, never offers 2400
--standard v21                               # shorthand for --modes v21
```

| Mode | Rate | Modulation | Carriers | Announced by |
|---|---|---|---|---|
| `bell103` | 300 bit/s | FSK | 1070/1270 and 2025/2225 Hz | 2225 Hz answer tone |
| `v21` | 300 bit/s | FSK | 980/1180 and 1650/1850 Hz | 2100 Hz answer tone, then channel 2 mark |
| `bell212a` | 1200 bit/s | 4-DPSK, 600 Bd | 1200 and 2400 Hz | 2225 Hz answer tone |
| `v22` | 1200 bit/s | 4-DPSK, 600 Bd | 1200 and 2400 Hz | 2100 Hz answer tone, then unscrambled binary 1 |
| `v22bis` | 2400 bit/s | 16-QAM, 600 Bd | 1200 and 2400 Hz | as V.22, then the S1 rate exchange |

The list changes four things: what V.8bis advertises and accepts, which
signals the answering side probes with, which of Bell 103 and Bell 212A a
2225 Hz answer tone is answered with, and whether the S1 exchange that
raises V.22 to 2400 bit/s is offered at all.

## Why the modes group the way they do

Bell 212A and V.22 are the *same data pump*: 600 baud differential 4-PSK
on 1200 Hz (calling) and 2400 Hz (answering) carriers, the same
1 + x^-14 + x^-17 scrambler, the same dibit-to-phase-change table, and the
same 456 ms / 270 ms / 765 ms handshake timings. They differ only in what
the answering modem emits to announce itself: V.22 sends unscrambled
binary 1 on its carrier, Bell 212A sends a 2225 Hz tone. V.22 §6.3.1.1
says so explicitly in a note, and the same note is why a V.22 caller will
usually answer a 2225 Hz tone as well.

So modec has three probes, not five, and two of them lead to the same
place:

- the **V.22 probe** transmits unscrambled binary 1 (a 2250 Hz tone),
- the **V.21 probe** transmits channel 2 mark (1650 Hz),
- the **Bell probe** transmits 2225 Hz, which serves Bell 103 and
  Bell 212A alike because it is both the Bell answer tone and the Bell 103
  answer-channel mark.

What the caller sends back, not what we sent, decides the outcome.

## Answering a call

1. **Billing delay.** Silence for 2 s (V.25 allows 1.8 to 2.5).
2. **V.8bis**, if enabled and at least one ITU mode is in the list. Send
   CRe: 1375 + 2002 Hz for 400 ms at 12 dB below the data level, then
   400 Hz for 100 ms. Listen for up to 3 s.
   A modem with only Bell modes configured skips this and step 3 entirely,
   because a Bell modem never emits the ITU tones.
3. **Answer tone.** 2100 Hz for 3 s (V.25 allows 2.6 to 4.0), then 75 ms
   of silence (55 to 95).
4. **Probe rotation.** 1.5 s per probe, cycling through the probes the
   mode list needs, in the order V.22, V.21, Bell.

Throughout steps 3 and 4, whatever we happen to be transmitting:

- a **Bell 103 originate carrier** (1270 Hz mark held for 300 ms) connects
  Bell 103 at once, because a Bell 103 caller transmits continuously and
  should not have to wait for the rotation to come round;
- **scrambled DPSK marks on the low channel**, held for 270 ms, connect at
  1200 bit/s;
- the **S1 pattern** (alternating 00/11 dibits) starts the V.22bis rate
  exchange.

## Placing a call

modec listens and takes the first thing it recognises:

| Heard | Meaning | Response |
|---|---|---|
| CRe (1375 + 2002 Hz, then 400 Hz) | a V.8bis modem | ESr, then a CL capabilities list |
| 2250 Hz with exact 270 degree phase steps | V.22 unscrambled binary 1 | the V.22 start-up |
| 2225 Hz without those phase steps, 155 ms | a Bell answer tone | Bell 212A if preferred, else Bell 103 |
| 2100 Hz for 300 ms | the ITU answer tone | wait for it to end, then take whatever carrier follows |

The 2225 and 2250 Hz cases are 25 Hz apart and a 40 ms tone window cannot
separate them, so the V.22 receiver settles it: unscrambled binary 1
produces exact 270 degree phase steps every symbol, while a plain tone at
2225 Hz produces steps about 15 degrees off. That measurement, not the
frequency, decides which of the two the caller is hearing.

## What happens when modec is called by...

### a Bell 103 modem

It waits for 2225 Hz and replies with a 1270 Hz originate mark. Our Bell
probe emits exactly that tone, but the reply is accepted during any probe
and during the 2100 Hz answer tone as well, so the connection is made
within about 300 ms of its carrier appearing, wherever we are in the
rotation. Result: `CONNECT 300`, Bell 103.

### a V.22 modem

It stays silent until it detects unscrambled binary 1 for 155 ms, waits
456 ms, then transmits scrambled binary 1 on the low channel. Our V.22
probe emits unscrambled binary 1; our receiver qualifies its scrambled
marks for 270 ms, answers with high-band scrambled ones, and after 765 ms
the link is up. Result: `CONNECT 1200`, V.22.

If it happens to answer our *Bell* probe instead (the V.22 note permits
this), the same 1200 bit/s link is established from the Bell probe.

### a Bell 212A modem

Identical to the V.22 case with the Bell probe doing the announcing: it
waits for 2225 Hz, waits 456 ms, sends low-band scrambled marks; we
qualify them for 270 ms, answer with high-band scrambled marks, and after
765 ms the link is up. Result: `CONNECT 1200`, Bell 212A.

A Bell 212A caller and a Bell 103 caller are told apart purely by their
reply to the same 2225 Hz tone: an FSK carrier at 1070/1270 Hz means
Bell 103, DPSK scrambled marks on a 1200 Hz carrier mean Bell 212A.

### a modern V.8bis modem

It answers our CRe with ESr and a CL listing its capabilities. We
intersect that list with our own mode list, in our preference order, and
send MS naming the winner. As V.8bis §9.9.3 requires, the station that
*receives* MS becomes the answering modem, so the roles reverse: we become
the calling modem and the V.25 start-up runs with the mode already fixed,
with no probing at all. Result: whatever both sides support, usually
`CONNECT 2400`.

V.8bis has no codepoint for Bell 212A (it is not an ITU mode), so Bell 212A
is never advertised there; it is reachable only through the classic
start-up.

### something that never answers

The caller gives up after `hcTimeout` (45 s by default) and reports
`NO CARRIER`. A Bell 212A attempt that draws no response within 6 s is
remembered as failed, and the next pass answers the same 2225 Hz tone as
Bell 103 instead, so a 212A-capable modem still connects to a Bell 103
answerer.

## Timings

| Step | modec | Specification |
|---|---|---|
| Billing delay | 2.0 s | V.25: 1.8 to 2.5 s |
| ITU answer tone | 3.0 s | V.25: 2.6 to 4.0 s |
| Silence after the answer tone | 75 ms | V.25: 55 to 95 ms |
| V.8bis CRe segment 1 / segment 2 | 400 / 100 ms | V.8bis §7.1.2 |
| V.8bis response wait | 3 s | V.8bis §10.2.2 |
| Answer signal before the caller replies | 155 ms | V.22 §6.3.1.1 |
| Caller silence before scrambled marks | 456 ms | V.22 §6.3.1.1 |
| Scrambled marks qualification | 270 ms | V.22 §6.3.1.1 |
| Scrambled ones before data | 765 ms | V.22 §6.3.1.2 |
| S1 pattern | 100 ms | V.22bis §6.3.1.1 |
| 2400 bit/s transmit switch after S1 | 600 ms | V.22bis §6.3.1.1 |
| 16-way decisions after S1 | 450 ms | V.22bis §6.3.1.1 |
| Probe rotation | 1.5 s each | modec's own choice |
| FSK carrier qualification | 300 ms | V.21 §8.2: circuit 109 in 300 to 700 ms |
