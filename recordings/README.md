# Call recordings

Received audio (`--record-rx`) from real calls, named by the time they were
placed. All are 8 kHz 16-bit mono, the G.711 audio as it arrived from the
VoIP trunk. Read them back with `modec detect` and `modec probe`.

The far end of every call here is the answering modem on +1 929 470 2600,
reached through voip.ms. It steps through a fallback ladder rather than
offering one modulation: roughly four seconds of 2100 Hz answer tone with
phase reversals, three seconds of V.22 unscrambled binary 1, two seconds
of V.21 channel 2 mark, a calling tone, then it repeats.

| Recording | Modes offered | Result |
|---|---|---|
| `…T220633-v22bis-2400-banner.wav` | default (all) | **V.22bis 2400 connected.** Decodes with an error figure of 0.017 and yields `https://2600.network - Patton 3120 #1` and `uSerNaME:` cleanly |
| `…T221747-v22-481-teardown.wav` | default (all) | Call torn down mid-handshake by `481 Call/Transaction Does Not Exist`, caused by baresip's `medianat=stun` re-INVITE |
| `…T222358-bell103-attempt.wav` | `bell103` | No connection, correctly: the 2250 Hz the far end sends is V.22 unscrambled binary 1, not the 2225 Hz Bell answer tone |
| `…T224844-v21-300-login.wav` | `v21` | **V.21 300 connected.** Banner and prompt clean, and the username was echoed back correctly, so both directions were good |
| `…T224922-v22-1200.wav` | `v22` | Connected at 1200 then immediately lost carrier. Decodes badly at 1200 and only slightly better at 2400: forcing V.22 without the S1 rate exchange does not interoperate with this answerer |
| `…T225032-v22bis-2400.wav` | `v22bis,v22` | No connection on this attempt; the ladder timing did not line up |

## What this established

- V.22bis at 2400 and V.21 at 300 both connect and carry data in both
  directions. At 300 bit/s the far end echoed our typed username back
  exactly, which proves the transmit path end to end.
- Forcing 1200 bit/s does not work against this answerer.
- Bell 103 and Bell 212A are unreachable here because the far end never
  sends a Bell answer tone.
