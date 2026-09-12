# A bench PBX between the USR, the ATA and modec

[sip-options.md](sip-options.md) parks a local PBX as "the right answer
once several devices are involved, since it can hold the USR modem
behind an ATA as another extension." Three devices is that point. This
is the note for the config in [asterisk/](asterisk/).

The topology is one box and one switch:

```
  USR Courier/Sportster ---RJ11--- HT802 FXS 1 --\
                                                  \
  (spare: phone or 2nd modem) ---- HT802 FXS 2 ---- Asterisk ---- baresip ---- modec
                                                  /   PCMU        (2001)      PipeWire
                            optional voip.ms trunk /
```

USB-serial drives the USR's DB25 from the same machine, so a whole call
-- dial, train, transfer, hang up -- is scriptable from one place.

## Why a PBX and not direct-IP

The HT802 and baresip can call each other's IP addresses with no PBX at
all, and for a first "does any of this work" test that is the shorter
road. It gives up three things this bench actually wants.

**Call progress.** Direct IP gets a call up; it does not give ringback,
busy, reorder, or ring-no-answer. Those are what [Modec.Progress](../src/Modec/Progress.hs)
and the call-progress paths in [Modec.Hayes](../src/Modec/Hayes.hs) are
for, and they are currently exercised only by things this project made
up. Extensions 605, 606 and 607 make each of them a phone number.

**Recording.** With Asterisk in the media path, every call is written
out as two separate legs with no extra apparatus. That is the corpus
generator: [recordings/](../recordings) grows from real USR handshakes
rather than only from BBSes that still answer, and each file drops
straight into [Modec.Replay](../src/Modec/Replay.hs).

**One place to put the impairments.** A trunk, a jitter buffer, a codec
choice -- all of them become dialplan, and therefore all of them become
things that are on or off by choice rather than by accident. That is the
same argument [Modec.Loopback](../src/Modec/Loopback.hs) makes for using
[Modec.Channel](../src/Modec/Channel.hs) instead of hand-rolled noise:
what a mode survives should be sayable in units someone chose.

## The recording tap is free

`MixMonitor` attaching an audiohook drags the bridge out of native RTP
forwarding into a simple bridge, which decodes each frame to linear and
re-encodes it for the far leg. That sounds like exactly the kind of
quiet damage this bench exists to avoid, and it is not: mu-law to linear
to mu-law is a bijection over the 256 codepoints. The tap costs a little
CPU and nothing else. [Modec.G711](../src/Modec/G711.hs)'s tables will
say so if it is worth checking.

What is *not* free is any codec mismatch, which is why every endpoint is
`disallow=all` / `allow=ulaw` and [asterisk/modules.conf](asterisk/modules.conf)
unloads the compressed codecs outright. A leg that cannot do mu-law
should fail the call loudly instead of being helpfully transcoded.

## What Asterisk does not add

chan_pjsip has no jitter buffer unless the dialplan asks for one with
`JITTERBUFFER()`. This is worth knowing because it is the opposite of
the usual advice: here the default is the good case, and
[asterisk/extensions.conf](asterisk/extensions.conf) turns a buffer on
only behind the `_69X` prefix, where dialling 6902 means "200 ms fixed,
on purpose." baresip keeps its own 100-200 ms fixed buffer
([baresip/config](baresip/config)); that one is wanted, since it absorbs
the network, and a modem trades latency for correctness gladly.

## HT802 settings that decide it

Per FXS port, under Settings -> FXS Port 1 / 2. Everything here is a
speech default that is wrong for data:

| Setting | Value | Why |
| --- | --- | --- |
| Preferred Vocoder 1 | PCMU | and leave 2-8 blank |
| Voice Frames per TX | 2 | 20 ms, matching the loopback block size |
| Silence Suppression | No | CNG during a V.8 silence wrecks [Modec.V8](../src/Modec/V8.hs) |
| Disable Line Echo Canceller | No | keep it on -- and run modec with `--ans-plain`, or the V.25 reversals in its answer tone tell the ATA's canceller to stand down for the call, and the hybrid's reflection comes back through the softphone 1.2 s later where modec's own canceller cannot reach it. See reference-modem.md, "The fix was one bit in the answer tone" |
| Disable Network Echo Suppressor | Yes | |
| Jitter Buffer Type | Fixed | adaptive buffers insert and drop samples |
| Jitter Buffer Length | High | latency is free, dropped samples are not |
| Fax Mode | Pass-Through | never T.38 |
| Re-INVITE After Fax Tone Detected | No | nothing may switch mid-call |
| Send DTMF | In-audio only | maximum transparency; digits are dialled by the ATA's own digit map anyway |
| Disable Call Waiting | Yes | a call-waiting beep mid-call destroys the link |
| Enable Call Features / Hook Flash | No | no feature can fire by accident |
| SLIC Setting | USA | |
| TX / RX Gain | 0 dB to start | then set from extension 600, not by ear |
| NAT Traversal | No | |
| Use Random SIP Port | No | otherwise the port's SIP stack is on a random port and silent on 5060, which looks exactly like an ATA refusing calls from an unregistered proxy. `scripts/bench/sweep.py` finds the port itself; see reference-modem.md, "Roles reversed" |
| Dial Plan | `{ [6]xx \| [1-2]xxx \| 1xxxxxxxxxx }` | 3-digit tests, 4-digit peers, 11-digit trunk |

Register port 1 as 1001 and port 2 as 1002 against the Asterisk box.

## Modem settings

Two command lineages turn up on this bench and they disagree about
almost every letter. Find out which one is in front of you before
copying anything: `ATI3` and `ATI7` name the chipset on Conexant parts,
`ATI4` prints the whole profile on a USR.

### Conexant-class USB modems (CX93010 and friends)

Nearly every cheap "USB V.92 data/fax/voice modem" is a Conexant
CX93010, which is Rockwell lineage. It does its DSP on-chip, enumerates
as USB CDC-ACM, and needs no driver on Linux -- unlike the winmodems of
the era it imitates.

```
AT&F0           factory profile
AT&K3           RTS / CTS hardware flow control  (NOT the USR meaning of &K)
AT&C1 &D2       DCD follows carrier; DTR drops the call
ATS0=1          answer on the first ring
AT%C0           V.42bis compression off
AT\N1           direct mode: no error control, so Modec.Mnp is tested
                separately from the modulation rather than on top of it
AT+GCI=B5       country code (US).  +GCI=? lists them
AT&W0           save
```

`AT+MS` is the important one, and it is better than the USR's `&N`:

```
AT+MS=?                        which modulations this chip really has
AT+MS?                         what is set now
AT+MS=B103,0,300,300           Bell 103, automode OFF, pinned
AT+MS=V22B,0,2400,2400         V.22bis, pinned
AT+MS=V32B,0,9600,9600         V.32bis at 9600, no fallback
AT+MS=V34,0,2400,33600         V.34 with the usual range
```

The `0` is automode off. With it off at *both* ends, the call either
trains in the mode you named or fails -- no fallback and no retrain to
hide the answer, which is the whole point of asking.

V.90 and V.92 will never come up over this path. They need a digital
ISP at one end; two analogue ports facing each other cannot do better
than V.34, whatever the box says on the front.

### USR Courier / Sportster

A 90s external USR is DB25, so a DB9F-DB25M cable plus a USB-serial
adapter -- an FTDI one, since the full modem-control lines matter here
and the cheapest cables do not carry them all.

```
AT&F1           factory profile with hardware flow control
AT&H1 &R2 &I0   CTS / RTS on, XON/XOFF off
AT&B1           fixed DTE rate
ATS0=1          answer on the first ring
AT&K0           data compression off   (&K is compression here, not flow control)
AT&M0           error control off
AT&W0           save
```

`AT&N` locks the link rate and `AT&U` sets the floor, so `AT&N6 &U6`
pins a call to 9600 and refuses to fall back. The rate table differs
between Courier and Sportster ROMs; `AT&$` prints the modem's own, and
`ATI4` prints what is currently set. Trust those over any table written
down elsewhere, including this one.

`ATX3` blind-dials without waiting for dial tone. The ATA supplies dial
tone, so it should not be needed -- if it is, the FXS port is not giving
enough loop current for the DAA.

## Linux, before any of that works

`lsusb` first. A Conexant dongle is usually `0572:1340` or `0572:1329`,
binds `cdc_acm`, and appears as `/dev/ttyACM0`. If instead it wants
`slmodemd` it is a SmartLink softmodem and belongs in a drawer: the host
would be doing the DSP, which is this project's job.

Then, and this is the one that wastes an evening: **ModemManager will
grab `/dev/ttyACM0`** the moment it appears and start probing it with AT
commands, so the port is either busy or answering someone else's
questions.

```
systemctl mask --now ModemManager
```

or, to keep it for other devices, a udev rule setting
`ENV{ID_MM_DEVICE_IGNORE}="1"` for that vendor and product. After that,
`picocom -b 115200 /dev/ttyACM0` -- CDC-ACM ignores the rate, but
picocom wants one.

If the modem will not go off-hook, or reports `NO DIALTONE` against an
ATA that plainly has dial tone, suspect `AT+GCI` before suspecting the
line: the wrong country profile changes the DAA thresholds.

## What two real modems buy

The HT802 has two FXS ports and the dialplan already bridges them --
dialling 1002 from 1001 is a modem-to-modem call, recorded by
MixMonitor like any other. With automode off at both ends via `AT+MS`,
every recording in [recordings/](../recordings) arrives *labelled*: a
file produced by `AT+MS=V32B,0,9600,9600` on both modems is V.32bis by
construction, not by anyone's later guess. That is ground truth for
[Modec.Detect](../src/Modec/Detect.hs) and for the mode-selection in
[Modec.Handshake](../src/Modec/Handshake.hs), and it is the first time
this project has had any.

Voice mode is the more interesting half. `AT+FCLASS=8` streams the line
to and from the DTE as 8 kHz samples, and on this family it is genuinely
full duplex: `+VTR` ("Start Voice Transmission and Reception (Voice
Duplex)") is documented for the CX930xx-2x parts, and documented as
running *"without either acoustic echo cancellation or line echo
cancellation"* -- which is the sentence this project would have written
itself. `+VSM=?` reports every format at 8000 samples/s:

```
0    SIGNED PCM     8 bit
1    UNSIGNED PCM   8 bit
129  IMA ADPCM      4 bit
130  UNSIGNED PCM   8 bit
131  Mu-Law         8 bit      <- same codepoints as Modec.G711
132  A-Law          8 bit
133  14 bit PCM     14 bit     <- more resolution than the SIP path can carry
```

So the dongle is a telephone-line sound card: a real DAA, off-hook and
dialling under `+VLS=1`, with modec's samples going straight in and out
over USB. No RTP, no jitter buffer, no second clock -- transmit and
receive share the modem's one 8 kHz codec clock, which is a materially
better place to stand than the ATA path, where baresip's clock and the
HT802's are strangers. It is also the answer to having no capture device
on this box, and it makes `133, 14 bit PCM` available, which is a
cleaner front end than anything G.711 can deliver.

`--audio-serial /dev/ttyACM0` is this in modec: the dialogue in
[Modec.Voice](../src/Modec/Voice.hs) -- `AT+FCLASS=8`,
`+VSM=<format>,8000`, `+VSD=0,0`, `+VIT=0`, `+VPR=0`, then `+VLS=1` and
`+VTR` -- the `<DLE>` shielding taken off the stream, and the modem run
on the samples, in `pcm14` unless `--audio-format` says otherwise. The
dialogue is data, and the log names the step that fails. Bring-up on
the real part has now happened, on a `CX93001-EIS_V0.2013-V92` behind
an HT802V2, and this is what it found:

1. `AT+FCLASS=?` answers `0,1,1.0,2,8`, so voice is there. **`AT+VTR=?`
   returns ERROR, and that means nothing at all.** This firmware has no
   test form for any *action* command -- `AT+VRX=?` and `AT+VTX=?`
   error the same way, and those are mandatory in Class 8 -- while every
   parameter command (`+VSM`, `+VSD`, `+VIT`, `+VTS`, `+VGT`, `+VPR`)
   answers its `=?` properly. The only test of `+VTR` is `+VTR` itself,
   off hook: it answers `CONNECT` and streams both ways.
2. `AT+VSM=?` lists exactly the table above, 133 included.
3. `--audio-format ulaw`: one byte a sample, the framing at half the
   byte rate, and a decoder checked against the Sun reference. This is
   now the default for `--audio-serial`, for the reason in 5.
4. `pcm14` is **left-justified**, so the scale in `Modec.Sample` is
   32768 and not 8192. Measured rather than assumed: held off hook on
   dial tone, the little-endian word peaks at 17659, which is more than
   twice the 8191 a right-justified value could reach, and 99.1 % of
   the energy sits in the 350 and 440 Hz bins, so the alignment is not
   in doubt either. The 14 bits sit in bits 15..2; bit 0 is set in
   every sample and bit 1 in two of sixteen thousand, which is a
   word-framing marker rather than signal.
5. **14-bit PCM works one direction at a time and not both.** The
   CX93001 has a single throughput budget shared by the two
   directions, about **30.4 kB/s**. Receiving alone, `pcm14` runs at
   its full 16.5 kB/s with one underrun. It is transmitting at the
   same time that breaks it: 16 kB/s each way is 32 kB/s, and that
   does not fit. Varying only the transmit load, with the receive side
   always asking for 16 kB/s:

   | transmit | receive | total | underruns in 6 s |
   | --- | --- | --- | --- |
   | 0 | 16493 | 16493 | 1 |
   | 4005 | 16475 | 20480 | 1493 |
   | 8011 | 16479 | 24490 | 2976 |
   | 12013 | 16106 | 28120 | 4440 |
   | 16018 | 14396 | 30414 | 5921 |

   Mu-law is 8 kB/s each way, 16 kB/s the pair, and runs clean: 1
   underrun and 0 overruns against `pcm14`'s 9611 and 1476 over a call
   of the same length. So `--audio-format pcm14` is right for a
   recording and wrong for a call.

   Neither knob that looks like the answer is one. The DTE rate is not
   the throttle -- B115200, B230400, B460800 and B921600 all measure
   the same 14.4 kB/s -- and neither is `+VPR`, at 0, 48 or 96. That is
   what CDC-ACM ignoring its own line coding looks like: the rate is
   nominal, and the limit is in the part.
6. A run interrupted mid-stream leaves the modem in the voice duplex
   state, and it answers the next run's first `AT` with the tail of the
   old one. `Serial.resync` sends `<DLE><ETX>`, `<DLE><^>` and a bare
   carriage return before the dialogue, so this no longer costs every
   second run.

The four things the driver does for the modem that it would not work
without:

- **`+VSD`** silence detection will cheerfully end the stream on quiet.
  Turn it off, or a handshake's silence intervals truncate the call.
- **`+VIT=0`** disables the inactivity timer, the other way the duplex
  state ends by itself.
- **`+VPR`** pins the DTE rate and turns off autobaud. Mu-law duplex is
  64 kbit/s each way and 14-bit PCM is 128 kbit/s each way; CDC-ACM
  carries that, but nothing should be renegotiating underneath it.
- **DLE shielding**: `0x10` in the sample stream is doubled in both
  directions, `<DLE><ETX>` ends the modem's flow, and `<DLE><^>` from
  the DTE leaves the duplex state. The byte rate is therefore
  data-dependent, and a de-stuffer belongs in the driver before anything
  else looks at the samples.

Whether a given dongle's firmware ships voice at all is worth one
command: `AT+FCLASS=?` should list `8`.

Class 1 fax (`AT+FCLASS=1`) is a further curiosity: the modem does the
modulation and the host does T.30, which would land on
[Modec.Hdlc](../src/Modec/Hdlc.hs). Not now, but the hardware is a
reference implementation of V.21 channel 2 sitting on the desk.

## Bring-up order

1. `pjsip show endpoints` -- 1001, 1002, 2001 all Avail.
2. From a modem, `ATDT600`. It should stay off-hook on a steady tone.
   Record it and measure: that number sets the gains.
3. `ATDT601` and speak into the spare port -- confirms audio both ways
   and shows the round-trip delay.
4. From modec, dial 1001 with the modem at `ATS0=1`. Ring, answer,
   handshake.
5. `ATDT605`, `606`, `607` from modec for the progress tones.
6. Only then the trunk.

Useful while it is not working: `pjsip set logger on`, `rtp set debug
on`, and `core show channel <name>` -- if NativeFormats, ReadFormat and
WriteFormat are not all `ulaw`, something is transcoding and the rest of
the bench is measuring the wrong thing.

## What this still does not give

The ATA's 8 kHz codec clock free-runs against modec's sample counter. At
a typical 100 ppm that is roughly 48 samples of slip per minute of call,
resolved silently by whichever buffer notices first. So a failure here
is ambiguous in a way a failure in [Modec.Loopback](../src/Modec/Loopback.hs)
is not. This bench answers "does it work against real hardware over real
VoIP"; it does not answer "at what SNR does it stop working." Keep the
numbers coming from the channel simulator.
