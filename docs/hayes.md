# The AT command set and the serial port

modec's Hayes mode (`--hayes`, implied by `--sip`) puts an AT command
interpreter between the terminal side and the line. This page is the
reference for what it accepts, and for putting it on a pseudo-terminal
that a terminal program opens as if it were a modem's serial port.

## Putting the modem on a serial port

```
modec modem --hayes --sip 127.0.0.1:4444 --pty-link /tmp/modem
```

`--data-pty` creates a pseudo-terminal and prints its path
(`/dev/pts/N`) on stdout. `--pty-link PATH` also creates a symlink to it,
so the port name in the terminal program stays the same from run to run.
Either flag alone is enough, and `modec dial` and `modec answer` take them
in place of `--listen`. The link is removed when modec exits. modec will
not replace a regular file at that path.

Point the terminal program at the link:

| program | setting |
| --- | --- |
| picocom | `picocom /tmp/modem`; the speed is ignored |
| minicom | `minicom -D /tmp/modem`, with hardware flow control off |
| SyncTERM | the modem device in its settings set to `/tmp/modem`, then a Modem entry in the dialling directory |
| DOSBox-X | a `directserial` port whose `realport` names the pseudo-terminal device |
| socat | `socat - FILE:/tmp/modem,raw,echo=0,clocal=1` |

What a pseudo-terminal carries, and what it does not:

- **DTR, one way.** Opening the port raises DTR and closing it drops it.
  `&D` decides what a drop does. The default, `&D2`, hangs up. While
  nothing has the port open the modem does not answer calls unless `&D0`
  is set, and anything it would send the terminal is thrown away and
  counted in the log.
- **No DCD and no RI.** A pseudo-terminal has no carrier line to raise.
  Tell the terminal program to ignore carrier detect, and watch for
  `CONNECT` and `NO CARRIER` instead. `&C` is stored and does nothing.
- **No speed and no flow control.** Bytes go through as fast as they
  arrive whatever speed the program sets. `&K` is stored and does nothing.

## Dialling

`ATD` takes the rest of the line as the dial string, keeping its case.

| typed | dials |
| --- | --- |
| `ATDT5551234`, `ATD555-1234` | a telephone number. On an audio line the digits go out as DTMF. Over SIP they become `sip:5551234@DOMAIN` with `--sip-domain` |
| `ATD1001@192.168.30.105:22097` | the SIP address `sip:1001@192.168.30.105:22097` |
| `ATDsip:bbs@example.org` | that URI, as typed. `sips:` works too |
| `ATDL` | the last dial string again |
| `ATDS=1` | the number stored with `AT&Z1=` |
| `A/` | the whole last command line again, without a carriage return |

A leading `T` or `P` is dropped when a digit or a `sip:` follows it. So
`ATDT1001@host` reaches `sip:1001@host` while `ATDtom@host` keeps its
user part. A user part that is a `T` or `P` followed by digits needs the
scheme written out: `ATDsip:T100@host`.

`W` and `!` are ignored. A comma pauses for S8 seconds. A trailing `;` is
ignored.

### SIP addresses: what works

SIP calls go through baresip (`--sip`), so any address baresip can reach
works. That includes a device on a random port, which is how the bench
dials its ATA. Native SIP inside modec is not implemented.

The limit is usually the terminal program. Many dialling directories take
only digits, and some upper-case what they send. For those, give modec a
phone book:

```
# /home/me/.modec-phonebook: what the terminal dials, then what to dial
5551234   sip:bbs@example.org
1001      1001@192.168.30.105:22097
```

```
modec modem --hayes --sip 127.0.0.1:4444 --pty-link /tmp/modem --phonebook ~/.modec-phonebook
```

A dialled number matches an entry on its digits alone, so `ATDT555-1234`
finds `5551234`. `AT&Zn=` stores four entries in memory for `ATDS=n`.

On an audio line, meaning PipeWire, pipes or a voice-mode modem, a SIP
address cannot be reached. modec says so in the log and reports
`NO CARRIER` rather than dialling its digits as tones.

## Choosing the modulation

`AT+MS=<carrier>,<automode>,<min rate>,<max rate>[,<min rx>,<max rx>]`,
with the carrier names Conexant and Rockwell modems use:

| carrier | modulation |
| --- | --- |
| `B103` | Bell 103 |
| `B212` | Bell 212A |
| `V21` | V.21 |
| `V22` | V.22 |
| `V22B` | V.22bis |
| `V23C` | V.23 |
| `V32` | V.32 |
| `V32B` | V.32bis |

- With automode 1, the default, a call can use that carrier and every one
  below it, in the order V.32bis, V.32, V.22bis, V.22, Bell 212A, V.21,
  Bell 103. With automode 0 it uses that carrier only. V.23 is reached
  only by naming it.
- The rates drop modulations that cannot run inside the range. On V.32 and
  V.32bis they also narrow the rates offered in the rate signal, so
  `AT+MS=V32B,0,4800,9600` connects at 9600 against a far end that offers
  14400. Equal rates pin the call to one rate. The receive rates are
  accepted and follow the transmit ones.
- Empty fields keep their defaults: `AT+MS=V22B` is V.22bis and below at
  any rate.
- `AT+MS?` reads the setting back. `AT+MS=?` lists what is accepted.
- Extended commands end at a `;`: `AT+MS=V32B;S7=60`.

`ATB1` prefers Bell 212A and Bell 103 over V.22 and V.21 where both are
allowed. `ATB0`, the default, prefers the ITU pair. `ATN0` connects at the
first carrier only, like automode 0, and `ATN1` steps down.

The profile modec starts with is the command line's. `--mode`,
`--v32-rate`, `--mnp` and `--ignore-busy` set the modes, the rate, `\N`
and `X`. `AT&F` goes back to those settings rather than to some other
modem's factory defaults.

## Commands

| command | effect |
| --- | --- |
| `A` | answer |
| `A/` | repeat the last command line |
| `Bn` | 0 ITU first, 1 Bell first |
| `D...` | dial; see above |
| `En` | echo commands, 1 by default |
| `H0` | hang up. `H1` is accepted and does nothing |
| `In` | 0 product, 3 version, 4 capabilities |
| `Ln`, `Mn` | speaker; accepted |
| `Nn` | 0 first carrier only, 1 step down |
| `O` | back to data mode, repeating `CONNECT` |
| `Qn` | 1 suppresses result codes |
| `Sn=v`, `Sn?` | S-registers 0 to 255 |
| `Vn` | 0 numeric result codes, 1 words |
| `Xn` | result code set; see below |
| `Z` | hang up, restore the `&W` profile |
| `&Cn` | stored |
| `&Dn` | DTR drop: 0 ignore, 1 command mode, 2 hang up, 3 hang up and `Z` |
| `&F` | the command line's profile |
| `&Kn` | stored |
| `&V` | show the active and stored profiles and the stored numbers |
| `&W` | store the active profile, until modec exits |
| `&Y` | accepted |
| `&Zn=s`, `&Zn?` | store or show number n, 0 to 3 |
| `\Nn` | 0 and 1 no error control. 2, 3 and 5 MNP classes 2 to 4, carrying on unprotected if the far end has none. 4 (LAPM) is an error |
| `%Cn` | stored; modec has no compression |
| `+MS` | modulation; see above |
| `+FCLASS` | `0` only: data |
| `+GMI`, `+GMM`, `+GMR`, `+GCAP` | identification |

Other `&`, `\` and `%` commands with a letter are accepted and ignored,
so initialisation strings written for other modems do not fail. Other
letters and unknown `+` commands are an error.

Result codes, with their `ATV0` numbers:

| code | number | when |
| --- | --- | --- |
| `OK` | 0 | |
| `CONNECT` | 1 | `X0` |
| `CONNECT 300` | 1 | |
| `CONNECT 1200` | 5 | |
| `CONNECT 2400` | 10 | |
| `CONNECT 4800` | 11 | |
| `CONNECT 7200` | 13 | |
| `CONNECT 9600` | 12 | |
| `CONNECT 12000` | 14 | |
| `CONNECT 14400` | 15 | |
| `RING` | 2 | |
| `NO CARRIER` | 3 | |
| `ERROR` | 4 | |
| `BUSY` | 7 | `X3` and `X4`; busy, congestion or a special information tone |
| `NO ANSWER` | 8 | over SIP, a call that was never established |
| `PROTOCOL: MNP CLASS n` | none | after `CONNECT`, in word mode |

At `X0` to `X2` modec does not listen for a busy line. It stays on until
S7 runs out and reports `NO CARRIER`. `NO DIALTONE` is never reported,
because modec dials without listening for dial tone.

## S-registers

Every register from 0 to 255 reads back what was written. These do
something:

| register | default | meaning |
| --- | --- | --- |
| S0 | 0 | rings before answering; 0 does not answer |
| S1 | 0 | rings so far; cleared 8 s after the last ring |
| S2 | 43 | escape character, `+`; above 127 disables the escape |
| S3 | 13 | carriage return: ends a command, and ends result lines |
| S4 | 10 | line feed in result lines |
| S5 | 8 | backspace |
| S6 | 2 | seconds of silence before the first DTMF digit |
| S7 | 45 | seconds to wait for the handshake to finish |
| S8 | 2 | seconds a comma pauses |
| S10 | 5 | tenths of a second of lost carrier before hanging up; 255 never |
| S11 | 80 | milliseconds of each DTMF digit, and of the gap after it |
| S12 | 50 | escape guard time, in fiftieths of a second |

S7, S10 and S11 keep the timings modec has always used, which differ from
a Rockwell modem's 50, 14 and 95.

On an audio line a ring is a burst of line energy; over SIP it is
baresip's report of an incoming call, repeated every two seconds. S0
counts either.
