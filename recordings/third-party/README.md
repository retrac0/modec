# Recordings someone else made

Calls placed by other people, on other modems, and published with the
audio. They are worth more than our own recordings for one reason: no
part of them came out of this code, so a fixture minted from one cannot
pass by agreeing with a mistake modec makes twice.

Both sets come from Gough Lui, who recorded them the same way we would
have: an analogue modem into an ATA, the call reassembled from the RTP
in both directions. So each file is 8 kHz 16-bit stereo with **one
direction per channel** -- which is the only form a V.32 recording can
usefully take, since both directions share the 1800 Hz carrier and a
line tap mixes them past separating.

| file | call | source |
|---|---|---|
| `banksia-mymodem144-v32bis-14400.wav` | V.32bis 14400, Banksia MyModem 144 (Rockwell) | [The (large) collection of V.34 modem sounds](https://goughlui.com/2017/05/30/project-the-large-collection-of-v-34-modem-sounds/) |
| `netcomm-2400sa-300bps.wav` | Bell 103 300, NetComm SmartModem 2400SA | [Tech flashback: NetComm SmartModem 2400SA](https://goughlui.com/2013/09/12/tech-flashback-netcomm-smartmodem-2400sa/) |
| `netcomm-2400sa-1200bps.wav` | V.22 1200, same modem | as above |
| `netcomm-2400sa-2400bps.wav` | V.22bis 2400, same modem | as above |
| `netcomm-2400sa-bell202.wav` | Bell 202, refused: the modulation is not permitted in Australia | as above |

**The channel order is not the same in the two sets, and nothing in a
WAV file says which is which.** The V.34 collection puts the
*originating* modem on the left; the SmartModem page puts the
*answering* modem there. Getting it backwards does not fail loudly --
`readWav` keeps the first channel and says nothing -- so the far end of
each call is split out here as its own mono file, and that is what the
fixtures are minted from:

    sox banksia-mymodem144-v32bis-14400.wav -c1 banksia-mymodem144-answer.wav remix 2
    sox banksia-mymodem144-v32bis-14400.wav -c1 banksia-mymodem144-call.wav   remix 1
    sox netcomm-2400sa-300bps.wav           -c1 netcomm-2400sa-300bps-answer.wav remix 1

`banksia-mymodem144-call.wav` is the odd one: it is a calling modem, not
an answering one, so it is the only recording here modec can *answer*.
Every other fixture in the corpus is `role: originate`, because a
recording cannot answer a modem that speaks first and every call we
placed ourselves has us speaking first.

The far end of all four SmartModem calls is the same Cisco, which is why
they share an `expect:` line.
