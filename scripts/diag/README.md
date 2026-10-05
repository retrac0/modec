# Receiver diagnostics

Reading a recording back through the whole modem is `modec replay`, a
subcommand rather than a script:

    cabal run modec -- replay --mode v22bis,v22 [--v8] [--seconds N] FILE.wav

A V.32 call's echo canceller only meets the call that happened if it is
given what was really sent, so replay a call that has a `-tx.wav` beside
it with that as the reference:

    cabal run modec -- replay --mode v32bis --line --truth \
      --tx recordings/STAMP-1001-tx.wav recordings/STAMP-1001.wav

Without `--tx` a replay regenerates its transmit, which stops matching
at the first payload byte, and leaves data-mode cancelling off.

`MODEC_MNP_TRACE=1`, on a replay or a live call, lists every MNP frame
taken off the line and every one put on it: type, sequence number,
credit, and for an information frame its text. On a live call it goes
in the call's log, so mind what was typed.

What follows are standalone tools for taking the signal apart below that
level. Build any of them against the library:

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
- `echolag.py STEM [FROM TO STEP]` -- where our own echo is in a recorded
  call, two seconds at a time, from `STEM-tx.wav` against `STEM.wav`. A
  delay that steps mid-call is silence that went into our transmit, by
  exactly the step; no lag standing out is no echo.
- `iotrace.py FILE [FROM TO]` -- a `MODEC_IO_TRACE` taken apart: how
  capture arrived, the capture clock after each arrival, and whether the
  loop kept up with it.
- `heapcall.py RECORDING.wav [modem options]`, or `heapcall.py --s0` --
  the live modem loop (not `replay`, which keeps everything) over a
  recorded call as fast as it goes, with the runtime's heap census on:
  how much was live through the run, what it was made of at the peak,
  and the longest collection. A heap that climbs for as long as a phase
  lasts is a call that gets later the longer it stays in it. A live
  modem takes the same census with `GHCRTS="-hT -i0.5 -s"` in its
  environment, and leaves `modec.hp` where it was started.
- `pwstall.py SECONDS [rt] [group NAME] [force FRAMES] [meter] [poke]
  [load N]` -- the softphone path's audio with no modem and no telephone
  in it: a loopback and two pw-cats, every arrival stamped, pw-mon's
  events beside them. Says how much capture the graph lost and when,
  which driver and quantum it ran on, and what the graph was doing at
  the time. `rt`, `group pipewire.dummy` and `force 960` are what modec
  does; without them it shows what each is for.
