# Carrying modem calls over SIP

Modem tones have to reach a real telephone line somehow. This note
records what the path has to provide, which of the four ways of getting
there was taken, and why the other three were not.
[voipms.md](voipms.md) is the walkthrough for setting one up.

## What the path has to provide

What people running vintage modems over VoIP report
([gridbugs](https://www.gridbugs.org/dial-up-over-voip-with-a-commodore-modem-from-1985/),
[Hackaday](https://hackaday.com/2024/12/19/getting-dial-up-to-work-over-voip-isnt-always-easy/),
[Modem over VoIP](https://en.wikipedia.org/wiki/Modem_over_VoIP)):

- **G.711 only** (PCMU or PCMA, 8 kHz, 20 ms packets). Any compressed
  codec destroys modem tones. Offer nothing else in the SDP.
- **No voice processing**: no echo cancellation, no silence suppression
  or VAD or comfort noise, no automatic gain. modec brings its own AGC
  and slicer.
- **A generous jitter buffer.** Its cost is latency, which a modem does
  not care about; corrupted bytes, which is what the alternative gives,
  it cares about a great deal.
- **Rates**: 300, 1200 and 2400 bit/s are reported solid over G.711,
  4800 marginal, faster worse. That is the range modec covers.
- **A passthrough trunk**: some carriers detect modem or fax tones and
  switch the call to T.38 or a voice-band-data mode. This has to be
  tried per provider.

## What was chosen: an external SIP user agent

baresip next to modec, audio between them over PipeWire, control over
baresip's `ctrl_tcp` module. baresip owns registration, INVITE/BYE, SDP,
NAT and DTMF; modec never speaks SIP. `modec modem --sip HOST:PORT
--audio-sip-loop PREFIX` and `modec dial NUMBER` drive it, mapping ATD,
ATA and ATH to `dial`, `accept` and `hangup`, and `CALL_ESTABLISHED`,
`CALL_CLOSED` and `CALL_INCOMING` to CONNECT, NO CARRIER and RING --
which is also real ring detection, something a sound card cannot give.

The audio is two `pw-loopback` instances (`PREFIX-to-sip` /
`PREFIX-line` and `sip-to-PREFIX` / `PREFIX-sip-line`), which present
baresip with nodes of exactly the classes its PipeWire module accepts.
pw-cat pins its streams to them by name with `--target`; that works for
nodes with ports, and bare `module-null-sink` nodes with a Source class
do not have them.

The configuration that matters -- G.711 only, a fixed 100–200 ms jitter
buffer, 8 kHz in and out, no `webrtc_aec`, no `augain` -- is in
[baresip/config](baresip/config), ready to copy to `~/.baresip`.

## What was not chosen

- **Our own SIP user agent in Haskell.** RTP with G.711 is small and
  worth writing eventually; SIP is not -- digest REGISTER, dialogs,
  offer/answer, re-INVITEs, NAT keep-alives and a long tail of provider
  quirks. No maintained Haskell SIP library exists (`hasip` is
  unfinished, `mediabus-rtp` is RTP only and from 2017). It buys the
  modem nothing.
- **A local PBX** (Asterisk or FreeSWITCH) with a trunk to the provider.
  The right answer once several devices are involved, since it can hold
  the USR modem behind an ATA as another extension and run the whole
  bench in one box. It still needs a SIP endpoint for modec, so it is
  option 1 plus a PBX, not instead of it.  Since built, once the USR
  and an ATA joined the bench: [asterisk.md](asterisk.md).
- **A hardware ATA** (Grandstream HT801-class: PCMU, echo cancellation
  off, jitter buffer maximum, silence suppression off, 0 dB gain). This
  is the right tool for the *hardware* modem and for reference
  recordings, but it does not put modec on VoIP by itself.

The test ladder for a first real call -- echo test, tone integrity,
modem to modem over the trunk, then a BBS -- is in
[voipms.md](voipms.md).
