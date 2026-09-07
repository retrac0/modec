# Dialling out through voip.ms

The plan from [sip-options.md](sip-options.md), made concrete: baresip
registers with voip.ms, modec drives baresip over its control port, and
the audio runs between them through PipeWire loopbacks. No sound card and
no ATA are involved.

## 1. voip.ms portal

**Create a sub-account for the modem** (Sub Accounts, Create Sub Account).
Using a sub-account rather than the main account keeps the modem's codec
settings separate from anything else on the account. Note the username it
gives you; it looks like `123456_modem`.

Set these on the sub-account:

| Setting | Value | Why |
|---|---|---|
| Allowed Codecs | **u-law only** | anything compressed destroys modem tones; G.711 is the only safe choice |
| DTMF Mode | RFC2833 (or AUTO) | out-of-band digits; modec dials through SIP anyway, so this matters little |
| NAT | Yes, if the machine is behind NAT | keeps the registration and RTP path open |
| Device Type | IP device / softphone | |

Leave any fax or T.38 option **off**. T.38 demodulates the signal as a
fax; a modem call must pass through as plain G.711 audio.

**Pick a POP server** near you from the portal's Servers page (they are
named like `toronto.voip.ms`, `seattle.voip.ms`, `newyork.voip.ms`). If
you later buy a DID for incoming calls, its POP must match.

**Add funds.** The two test numbers below work on a new account without
funds; real calls need a balance.

## 2. baresip

```
sudo pacman -S baresip
mkdir -p ~/.baresip && cp docs/baresip/config docs/baresip/accounts ~/.baresip/
```

Edit `~/.baresip/accounts` with your sub-account, password and POP:

```
<sip:123456_modem@toronto.voip.ms>;auth_pass=YOURPASSWORD;audio_codecs=PCMU/8000/1;answermode=manual;regint=300;pubint=0
```

`audio_codecs=PCMU/8000/1` keeps the SDP offer to G.711 µ-law alone, so a
codec is never negotiated that would wreck the tones. `answermode=manual`
leaves answering to modec, so `ATA` controls it.

Start baresip and check it registered:

```
baresip
/reginfo        # should show the account as registered
```

Leave it running. It listens for modec on 127.0.0.1:4444.

## 3. Placing a call

```
modec dial +14042820600
```

That is the whole thing. `dial` starts baresip if nothing is already
listening on its control port, takes the domain from the account in
`~/.baresip/accounts`, brings up the PipeWire loopback pair, dials, puts
this terminal on the modem in raw mode, and records the call. When the
call ends, so does modec; `--stay` keeps the AT prompt instead. `+++ATH`
hangs up from the keyboard, ctrl-C leaves.

Add `--listen 2323` to put the modem on a telnet port rather than the
terminal, which is what you want for a terminal emulator with ANSI and
file transfer.

On the terminal the line discipline is turned off entirely: the return
key reaches the far end as the carriage return it is rather than as a
line feed, ^S and ^Q go down the line instead of freezing the screen,
and the high bit survives. ctrl-C is the one key the terminal keeps, and
it leaves.

The long form is still there when you need to place the pieces yourself
-- an existing baresip, a different audio path, the answering side:

```
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain toronto.voip.ms \
  --audio-sip-loop modec --listen 2323
```

Then point a terminal program at `localhost 2323` and type AT commands.
modec creates the PipeWire loopback nodes baresip captures from and plays
into; baresip's `audio_source` and `audio_player` in the supplied config
already name them.

## 3a. What every call leaves behind

Each call, dialled or answered, writes three things under `recordings/`:

```
recordings/20260906T154925-+19719104722.wav   what came down the line
recordings/20260906T154925-+19719104722.log   what the modem made of it
recordings/calls.log                          one line per call
```

The log is stamped in seconds from the start of the call, so it reads
alongside the recording:

```
# 2026-09-06 15:49:25 EDT  +19719104722
   3.24  SIP call up, modem role Originate
  15.31  CONNECT V21 300 bit/s, sending v21-ch1, hearing v21-ch2
  28.08  call ended: connected V21 300 bit/s
```

`--record-dir DIR` moves them, `--no-record` turns them off. `--record-rx`
and `--record-tx` are a different thing and still there: one file for the
whole session rather than one per call, which is what a bench run wants.

## 4. Test ladder

Work up in this order; each step isolates one thing.

**a. Voice echo, no modem.** Dial `4443` from any softphone on the same
account and talk. This proves registration, RTP and the audio path. The
echo test needs no funds.

**b. Tone integrity, with modec.** Dial the echo test with modec and
record what comes back:

```
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain toronto.voip.ms \
  --audio-sip-loop modec --listen 2323 \
  --mode bell103 --no-handshake --record-rx echo.wav --record-tx sent.wav
```

The long form, because this test wants both directions in one file each
rather than the per-call recording `dial` makes. `ATDT4443`, let it run
ten seconds, then `+++` and `ATH`. The echo test
returns your own audio, so `echo.wav` is your own carrier after a round
trip through the trunk. Check it:

```
cabal run modec -- probe echo.wav      # is the 1270 Hz mark still clean?
cabal run modec -- detect echo.wav     # does it read as a Bell 103 channel?
```

A clean single tone at close to full amplitude means the trunk passes
modem tones. Warbling, dropouts or a smeared spectrum mean a codec or
packet loss problem, and no modem will work until that is fixed.

**c. Modem to modem over the trunk.** Create a second sub-account and a
second DID, run a second baresip and modec, and have one dial the other.
This is a full modem call over the real network with a known payload at
both ends, which is far easier to debug than a BBS.

**d. A real BBS.** Start slow and work up:

```
modec dial <number> --mode bell103    # 300 bit/s, most forgiving
modec dial <number> --mode v22        # 1200 bit/s
modec dial <number>                    # 2400 bit/s if both ends manage it
```

Every one of those is recorded without being asked; when a connection
fails, `modec detect recordings/<the call>.wav` next to that call's log
shows exactly how far the handshake got.

## Field notes from a real call

Confirmed working against voip.ms from this repository: registration on
`toronto.voip.ms` (30 ms round trip, 2.8 ms jitter, no loss), PCMU
negotiated both ways, and a **V.22bis connection at 2400 bit/s to a real
answering modem**, whose banner and prompts decoded perfectly.

Four things had to be fixed to get there, all of them worth knowing:

**`modec dial` asks for MNP error correction by default**, the way a
modem with its factory settings does, and `--no-mnp` turns it off. An
unprotected 2400 bit/s call over this trunk delivers the odd corrupt
character in the direction it transmits -- measured at four in a
two-minute call, against none in the calls that negotiated MNP class 4
over the same trunk the same evening. Nothing downstream can tell a
corrupt character from one that was typed, which is why it is worth the
framing.

Two notes for reading a recording back afterwards. A call that ran with
MNP has to be replayed with MNP: the frames are start-stop characters
like any other, and a replay without it prints them as pages of
garbage. And a BBS sending CP437 line art puts plenty of bytes above
0x7E on the line -- count those as noise and every ANSI screen looks
like a broken link.

**Junk characters echoed back by the far end -- `þ` (0xFE) at 300 bit/s,
random bytes at 2400 -- while what it sends reads perfectly** are holes
in our transmitted audio, not line noise. Each hole is a moment of
silence in our carrier that the far end's framer takes for a start bit.
modec keeps 100 ms of audio ahead of its playback stream for exactly
this reason; `MODEC_TX_LEAD_MS=200` widens it and `MODEC_PW_LATENCY=50ms`
changes the quantum both pw-cat streams run on, if a machine needs
different figures.

To measure it, run the modem on the loopback with no call up
(`modec modem --audio-sip-loop modec --originate --data-stdio`) and
watch `pw-top`: the `ERR` column on `modec-tx` counts the playback
stream's underruns, and it should not move once the first few seconds
are past. Ignore the burst at startup -- the stream connects before
capture delivers anything -- and ignore the column entirely during a
call: the sound card becomes the graph driver then, at a 5 ms cycle, and
the count climbs by the hundred on a stream that is delivering every
sample. The echo the far end gives you is the judge during a call.

**Do not set `medianat=stun` on the account.** baresip then sends a
re-INVITE once STUN resolves the media address, and voip.ms answers
`481 Call/Transaction Does Not Exist`, tearing the call down in the middle
of the handshake. The supplied `accounts` file omits it.

**Comment out `module_app stdio.so`** when running baresip headless; with
no terminal it blocks startup. The supplied `config` has it commented.

**PipeWire may link the default microphone into the softphone's capture**
alongside the modem line, putting room noise on the wire while the far end
tries to demodulate. modec now detects and removes such links when a call
comes up, and logs what it removed.

**Outbound calls need a registered caller ID.** Without a DID or a
validated caller ID, voip.ms answers, plays "the number you are calling
from has not been registered", and hangs up after about eight seconds.
The 4443 echo test works without one, so use it to prove the audio path
before sorting the caller ID out.

## Reading the far end's answer sequence

A recording plus `modec detect` tells you which modes an answering modem
is actually offering, which is the fastest way to know what can connect.
One modern answering modem tested here cycles through a ladder:

| Time | Signal | Meaning |
|---|---|---|
| 0-4 s | 2100 Hz in 0.9 s segments | V.25 answer tone with phase reversals (V.8 ANSam) |
| 4-7 s | 2250 Hz | V.22 unscrambled binary 1 |
| 7-9 s | 1650 Hz | V.21 channel 2 mark |
| 9-11 s | 1300 Hz | calling tone, then the ladder repeats |

Against that answerer, `--mode v22bis,v22` connects at 2400 bit/s and
`--mode v21` connects at 300, but the V.21 window is only about two
seconds wide before it moves on. `--mode bell103` and `--mode bell212a`
never connect, correctly: the 2250 Hz it sends is V.22 unscrambled binary
1, not the 2225 Hz Bell answer tone, and modec tells the two apart by the
phase-step quality rather than the frequency.

## What to expect

Reports from people running vintage modems over VoIP put 300, 1200 and
2400 bit/s in the reliable range over G.711, with 4800 marginal. That is
the whole range modec implements, so the trunk should not be the limit.
The usual failure is a provider or ATA quietly applying echo cancellation,
silence suppression or a compressed codec.

If a call connects and then drops, look at the `MODEC_TRACE=1` log: it
prints the V.22 decision-error figure per block, which rises before a
connection is lost and tells you whether the problem is the trunk or the
receiver.

## Transmit level: WirePlumber will quietly attenuate the modem

The one local fault that cost the most time. Sending was 7.5 dB low with
every control reading unity, so the far end heard our carrier but decoded
junk from it while our own recordings of what we sent were flawless.

WirePlumber restores per-application volumes from
`~/.local/state/wireplumber/stream-properties`, and the key it matched was
`Output/Audio:application.name:pw-cat`. Every `pw-cat` playback stream on
the machine shares that name, so one slider left at three quarters in a
mixer years ago (0.75 cubed is the 0.421824 that appears in the file) was
being reapplied to the modem's transmit stream. Nothing in a call points
at it: the node volume, the loopback volumes and the sink volumes all read
1.0, because the attenuation lives in `channelVolumes` rather than
`volume`.

modec now creates its `pw-cat` streams with `application.name = modec` and
`state.restore-props = false`, which opts out of the restore, and checks
the applied gain shortly after the streams appear:

```
modec: warning: modec-tx is -7.5 dB; audio levels will be wrong (reset it in a mixer)
```

A send level is part of the modulation rather than a listening preference,
so this is deliberately not a user-adjustable control. To confirm a chain
end to end, play a known tone through it and measure what comes back;
through a `pw-loopback` pair it should return within a decibel:

```
pw-loopback -n t --capture-props='{ media.class = Audio/Sink node.name = t-sink }' \
                 --playback-props='{ media.class = Audio/Source node.name = t-src }' &
pw-cat --record --target t-src --raw --rate 8000 --channels 1 --format s16 out.raw &
pw-cat --playback --target t-sink --rate 8000 --channels 1 --format s16 tone.wav
```

The same measurement showed the loopback itself is otherwise clean:
distortion and noise sit 83 dB below a 1004 Hz tone, and nothing escapes
the 300-3400 Hz band, so the 8 kHz to 48 kHz and back resampling is not
what was damaging the signal.
