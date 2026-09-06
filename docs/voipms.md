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

## 3. modec

```
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain toronto.voip.ms \
  --audio-sip-loop modec --hayes --listen 2323
```

Then point a terminal program at `localhost 2323` and type AT commands.
modec creates the PipeWire loopback nodes baresip captures from and plays
into; baresip's `audio_source` and `audio_player` in the supplied config
already name them.

## 4. Test ladder

Work up in this order; each step isolates one thing.

**a. Voice echo, no modem.** Dial `4443` from any softphone on the same
account and talk. This proves registration, RTP and the audio path. The
echo test needs no funds.

**b. Tone integrity, with modec.** Dial the echo test with modec and
record what comes back:

```
cabal run modec -- modem --sip 127.0.0.1:4444 --sip-domain toronto.voip.ms \
  --audio-sip-loop modec --hayes --listen 2323 \
  --modes bell103 --no-handshake --record-rx echo.wav --record-tx sent.wav
```

`ATDT4443`, let it run ten seconds, then `+++` and `ATH`. The echo test
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
ATDT<number>          # with --modes bell103   (300 bit/s, most forgiving)
ATDT<number>          # with --modes v22       (1200 bit/s)
ATDT<number>          # with the default modes (2400 bit/s if both ends manage it)
```

Always record with `--record-rx`; when a connection fails, the recording
plus `modec detect` shows exactly how far the handshake got.

## Field notes from a real call

Confirmed working against voip.ms from this repository: registration on
`toronto.voip.ms` (30 ms round trip, 2.8 ms jitter, no loss), PCMU
negotiated both ways, and a **V.22bis connection at 2400 bit/s to a real
answering modem**, whose banner and prompts decoded perfectly.

Four things had to be fixed to get there, all of them worth knowing:

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

Against that answerer, `--modes v22bis,v22` connects at 2400 bit/s and
`--modes v21` connects at 300, but the V.21 window is only about two
seconds wide before it moves on. `--modes bell103` and `--modes bell212a`
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
