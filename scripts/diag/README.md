# Receiver diagnostics

Standalone tools for taking a recording apart. Build any of them against
the library:

    cabal exec -- ghc -O1 -package modec -o /tmp/bits scripts/diag/bits.hs

- `bits.hs FILE.wav low|high 1200|2400` -- the descrambled bit stream:
  fraction of ones, run lengths, and whether the zero runs sit on a
  10-bit async grid (they do if we are mis-framing real characters, they
  do not if they are bit errors).
- `chars.hs FILE.wav low|high 1200|2400` -- the same path through the
  async framer, printed.
- `idle.hs` -- modec's own V.22 idle straight back into its own receiver,
  the control for everything above.
- `sps.hs FILE.wav` -- the timing loop's samples-per-symbol estimate and
  EVM, per second, to see the far end's clock offset and whether the
  receiver is locked.
- `ansam.hs FILE.wav...` -- where ANSam was detected in each recording.
