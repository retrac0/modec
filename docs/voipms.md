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
