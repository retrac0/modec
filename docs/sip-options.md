# SIP options for G.711 calls (and dialling real BBSes over VoIP)

Goal: place and receive calls over a VoIP service so that modec can talk to
real modems (BBSes, the USR modem behind an ATA) with the audio carried as
G.711 over RTP. This note compares the ways to get SIP into the picture and
recommends one.

## What the far end needs from us

Everything below is what people who run vintage modems over VoIP report
([gridbugs](https://www.gridbugs.org/dial-up-over-voip-with-a-commodore-modem-from-1985/),
[Hackaday](https://hackaday.com/2024/12/19/getting-dial-up-to-work-over-voip-isnt-always-easy/),
[Modem over VoIP](https://en.wikipedia.org/wiki/Modem_over_VoIP)):

- **Codec PCMU or PCMA only** (G.711, 8 kHz, 20 ms packets). Any compressed
  codec destroys modem tones. Offer nothing else in the SDP.
- **No voice processing on the path**: no echo cancellation on our side, no
  silence suppression / VAD / comfort noise, no automatic gain control.
  modec already runs its own adaptive slicer and AGC.
- **A jitter buffer, and a generous one.** With the jitter buffer off, bytes
  arrive corrupted; the fixed cost is latency, which does not matter for
  a modem. Our receivers already tolerate slips and small rate errors, but
  the fewer the better.
- **Rates**: 300, 1200 and 2400 bit/s are reported as solid over G.711;
  4800 is marginal; anything faster suffers. That matches exactly what
  modec implements.
- **Providers**: some carriers detect modem/fax tones and switch the call
  to T.38 or a "voice band data" mode; a plain G.711 passthrough trunk is
  what we want. This has to be tried per provider; a cheap DID from a
  provider that documents fax passthrough is the usual choice.

## Options

### 1. External SIP user agent, audio through PipeWire (recommended)

Run baresip next to modec. baresip is in the Arch repos (4.6.0) with the
modules we need: `pipewire.so` (native PipeWire audio node), `g711.so`,
`ctrl_tcp.so` (JSON commands over TCP for scripting), `stdio`/`cons`
(interactive), `aufile` (file audio for offline tests). It registers with
the provider, handles INVITE/BYE, SDP, NAT (STUN/ICE modules), SRTP if
wanted, and DTMF (RFC 2833 or in-band).

Wiring: modec's `--audio-pipewire` currently spawns `pw-cat`, which makes
it a PipeWire node; `pw-link` connects modec's output to baresip's input
node and baresip's output to modec's input. No sound card involved.
Scripting: modec's Hayes layer sends `dial`/`hangup` to baresip over
`ctrl_tcp` (port 4444, netstring-framed JSON such as
`{"command":"dial","params":"sip:+15551234567@provider"}`), and baresip's
events (`CALL_ESTABLISHED`, `CALL_CLOSED`, `CALL_INCOMING`) drive
CONNECT / NO CARRIER / RING. That also gives us real ring detection, which
a sound card cannot.

baresip config that matters:

```
module            pipewire.so
module            g711.so
module_app        ctrl_tcp.so
module_app        menu.so
audio_player      pipewire,modec
audio_source      pipewire,modec
ausrc_srate       8000
auplay_srate      8000
audio_codec       PCMU/8000/1
audio_codec       PCMA/8000/1
audio_jitter_buffer_type  fixed
audio_jitter_buffer_ms    100-200
ctrl_tcp_listen   127.0.0.1:4444
```

and no `webrtc_aec`, no `augain`, no silence-suppression modules loaded.

Cost: a few hundred lines in the modec executable (ctrl_tcp client, event
mapping, `pw-link` setup), no new DSP. Everything SIP-related that goes
wrong is debuggable with baresip's own console.

### 2. Our own SIP user agent in Haskell

RTP with G.711 is small (a 12-byte header, µ-law/A-law tables) and worth
having eventually for the "own codecs" direction. SIP itself is not
small: REGISTER with digest authentication, INVITE/ACK/BYE dialogs, SDP
offer/answer, re-INVITEs, NAT keep-alives, provider quirks. There is no
maintained Haskell SIP library: `hasip` is an unfinished GitHub project
and `mediabus-rtp` (RTP only) dates from 2017. Writing a minimal UA is a
week of careful work and a long tail of interoperability fixes; it buys
nothing for the modem itself.

Sensible middle path: implement RTP/G.711 in Haskell (cheap, useful for
tests and for receiving audio from a PBX with `Dial(RTP)`-style legs) but
leave SIP signalling to option 1 or 3.

### 3. A local PBX (Asterisk or FreeSWITCH) with a trunk to the provider

Both are in the AUR (asterisk 23.5, freeswitch 1.10.12). Asterisk can
present modec as an extension and the provider as a trunk, force G.711,
disable echo cancellation per channel, and log everything. It also lets
the USR modem join through an ATA as another extension, so the whole
modem-to-modem test bench runs in one box without any provider.

Cost: setup and maintenance of a PBX; audio into Asterisk from modec still
needs a SIP endpoint (baresip again, or Asterisk's `chan_console`/ALSA
channel driver with PipeWire's ALSA plugin). Worth it once several devices
are involved, overkill for the first calls.

### 4. Hardware ATA on the line side

A Grandstream HT801/HT802 registers with the provider itself and gives a
real telephone line to the USR modem (settings: PCMU, echo cancellation
off, jitter buffer maximum, silence suppression off, gain 0 dB; exactly
what gridbugs used). This is the right tool for the *hardware* modem, and
it produces the reference recordings we want. It does not connect modec
to VoIP by itself; modec would still need option 1 or 3, or the sound-card
line interface in `line-interface.md` plugged into the ATA's second port.

## Implementation (done)

`modec modem --sip HOST:PORT --audio-sip-loop PREFIX` implements option 1:
see the README and `docs/baresip/` for the configuration. Audio is
wired with two `pw-loopback` instances (PREFIX-to-sip / PREFIX-line and
sip-to-PREFIX / PREFIX-sip-line), which give baresip nodes of exactly the
classes its PipeWire module accepts. pw-cat pins its streams to those
nodes by name (`--target`), which works for nodes that have ports; bare
`module-null-sink` nodes with a Source class do not.

## Recommendation

1. Option 1 now: add a `--sip` mode to `modec modem` that drives baresip
   over ctrl_tcp and links audio with `pw-link`. Hayes `ATD` becomes a
   SIP dial, `ATA` answers an incoming SIP call, `ATH` hangs up, RING comes
   from `CALL_INCOMING`. First test target: the two modec instances
   calling each other through a local baresip pair (no provider), then a
   provider DID, then a BBS.
2. Keep an HT801-class ATA on the shopping list for the USR modem; it
   doubles as a second VoIP endpoint for modem-to-modem tests through the
   provider.
3. Write the RTP/G.711 codec in Haskell when the "own codecs" work starts;
   consider a home-grown SIP UA only if baresip proves limiting.

## Test plan for the first BBS call

- Verify the trunk passes tones: call the provider echo test with a
  2100 Hz tone and Bell 103 idle mark, record what comes back, run
  `modec detect` on it.
- Dial a well-known 2400 bit/s BBS (several are listed at telnetbbsguide
  with dial-up numbers) with `--standard v22`, then automode.
- Expect 300 and 1200 to work first; 2400 depends on the trunk's jitter.
  Keep `MODEC_TRACE=1` logs and the EVM figures for tuning.
