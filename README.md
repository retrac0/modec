# modec

A software audio modem written from scratch in Haskell. Audio in and out
via PipeWire (later SIP/RTP with G.711), bytes in and out as telnet
streams. Targets, in order: Bell 103, V.21, V.22, V.22bis, V.8bis link
establishment.

See [SURVEY.md](SURVEY.md) for the survey of existing work and the design
plan, and [docs/line-interface.md](docs/line-interface.md) for hooking a
real modem to a sound card.

## Status

- Bell 103 and V.21 asynchronous FSK modulator and demodulator, offline
  (whole-file), any sample rate. Cross-validated both ways against
  minimodem 0.24.
- WAV reader (PCM 8/16/24/32, float 32) and 16-bit mono writer.

## Usage

```
cabal build
cabal test
cabal run modec -- decode test/fixtures/bell103_ans_8k.wav       # auto channel
cabal run modec -- decode --originate --v21 file.wav
printf 'hello\r\n' | cabal run modec -- encode --answer -o out.wav
cabal run modec -- probe recording.wav                            # tone energies
```

## Test fixtures

`test/fixtures/*.wav` were generated with minimodem (`minimodem --tx -f
out.wav -R <rate> [-M mark -S space] 300 < text`); the matching `.txt`
holds the payload. `bell103_ans_8k_noisy.wav` has sox white noise mixed in.
