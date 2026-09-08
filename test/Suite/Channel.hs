-- | What the channel simulator does, measured against what it claims.
--
-- Every other use of "Modec.Channel" in this suite asks what a /modem/
-- survives.  These ask what the /channel/ does, which is a different
-- question and the one that decides whether the answer to the first
-- means anything: an impairment that degrades a signal convincingly
-- while modelling nothing is worse than no impairment at all, because
-- it produces a number.
--
-- So each case here drives one impairment with a signal whose response
-- has a closed form, and checks the measurement against the formula.
module Suite.Channel (channelSimTests) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Modec.Channel
import Modec.DSP
import Modec.G711

fs :: Double
fs = 8000

-- | A sine, @n@ samples of it.
tone :: Double -> Double -> Int -> Signal
tone hz amp n = VS.generate n (\i -> amp * sin (2 * pi * hz * fromIntegral i / fs))

-- | Amplitude of the component at @hz@, by one DFT bin.
at :: Double -> Signal -> Double
at hz x = toneAmplitude (VS.length x) (goertzel fs hz x)

-- | Signal to everything-else ratio of a round trip, in dB.
distortionDb :: (Double -> Double) -> Double -> Double -> Double
distortionDb f lvl hz =
  let x = tone hz (fromDb lvl * 0.9) 40000
      y = VS.map f x
  in 20 * logBase 10 (rms x / max 1e-30 (rms (VS.zipWith (-) y x)))

channelSimTests :: TestTree
channelSimTests = testGroup "what the channel does"
  [ testGroup "analogue distortion"
    [ testCase "the polynomial makes the harmonics its coefficients predict" $ do
        -- For x = A sin, the second harmonic comes out at a2*A/2 of the
        -- fundamental and the third at a3*A^2/4.  Both are exact.
        let a2 = 0.05; a3 = 0.2; amp = 0.6
            x = tone 1000 amp 8000
            y = applyChannel fs idealChannel { chNonlin = Just (Polynomial a2 a3) } x
            h1 = at 1000 y; h2 = at 2000 y; h3 = at 3000 y
        assertClose "second harmonic" 0.03 (a2 * amp / 2) (h2 / h1)
        assertClose "third harmonic" 0.05 (a3 * amp * amp / 4) (h3 / h1)

    , testCase "and no DC, because a transformer would not pass it" $ do
        -- a2*x^2 has a mean of a2*A^2/2, which is 0.009 here: small, and
        -- it would sit under every level measurement downstream.
        let x = tone 1000 0.6 8000
            y = applyChannel fs idealChannel { chNonlin = Just (Polynomial 0.05 0.2) } x
            mean = VS.sum y / fromIntegral (VS.length y)
        assertBool ("mean " ++ show mean) (abs mean < 1e-9)

    , testCase "the soft clip is odd, so it makes no second harmonic" $ do
        let x = tone 1000 0.8 8000
            y = applyChannel fs idealChannel { chNonlin = Just (SoftClip 3) } x
            h1 = at 1000 y; h2 = at 2000 y; h3 = at 3000 y
        assertBool ("second harmonic " ++ show (h2 / h1)) (h2 / h1 < 1e-6)
        assertBool ("third harmonic " ++ show (h3 / h1)) (h3 / h1 > 0.02)

    , testCase "and leaves a small signal alone" $ do
        -- The normalisation that matters: a distortion must not also be
        -- a gain.  tanh(d*x)/tanh(d) would be one, at 3 dB per unit of
        -- drive, and everything level-dependent downstream would drift.
        let x = tone 1000 0.001 8000
            y = applyChannel fs idealChannel { chNonlin = Just (SoftClip 5) } x
        assertClose "small-signal gain" 0.001 1 (at 1000 y / at 1000 x)

    , testCase "the polynomial cannot be driven into folding back" $ do
        -- Past a3 = 1/3 the curve turns over and the output falls as the
        -- input rises.  Asking for 0.8 gets 1/3.
        let out a3 = at 1000 (applyChannel fs idealChannel { chNonlin = Just (Polynomial 0 a3) } (tone 1000 0.9 8000))
        assertClose "clamped" 1e-9 (out 0.8) (out (1 / 3))
        assertBool "and still rises with drive" (out 0.1 > out (1 / 3))
    ]

  , testGroup "the digital span"
    [ testCase "companding holds its ratio where linear quantisation does not" $ do
        -- The whole argument for G.711 in one assertion.  Swept a
        -- decibel at a time from full scale to -40 dBFS, mu-law stays in
        -- a band; eight-bit linear loses a decibel for every decibel.
        let mus = [ distortionDb ulawRound lvl hz | lvl <- [0, -1 .. -40], hz <- [1004, 2100] ]
            lin v = fromIntegral (round (v * 128) :: Int) / 128
            -- stopping at -30: by -40 dBFS an eight-bit linear
            -- quantiser has the signal down to about one step, and the
            -- law stops being a law
            linSnrs = [ distortionDb lin lvl 1004 | lvl <- [0, -10, -20, -30] ]
        assertBool ("mu-law band " ++ show (minimum mus, maximum mus))
          (minimum mus > 31 && maximum mus < 40)
        assertBool ("linear falls about a decibel per decibel: " ++ show linSnrs)
          (all (\d -> abs (d - 10) < 2) (zipWith (-) linSnrs (drop 1 linSnrs)))

    , testCase "all ones is silence and all zeros is full scale" $ do
        -- Which is why a stuck span is modelled at the code level.  Do
        -- it at the sample level -- "the samples go to 1.0" -- and the
        -- one case that really happens comes out as the opposite of
        -- what it is.
        assertEqual "0xFF" 0 (ulawDecode 0xFF)
        assertBool "0x00" (ulawDecode 0x00 < -0.97)

    , testCase "every code survives a decode and re-encode, except the second zero" $ do
        -- mu-law has two zeros, 0xFF and 0x7F, and they both decode to
        -- it; nothing else may be lossy.
        let bad = [ c | c <- [0 .. 255 :: Word8], ulawEncode (ulawDecode c) /= c ]
        assertEqual ("codes: " ++ show bad) [0x7F] bad
        assertEqual "A-law loses none" [] [ c | c <- [0 .. 255 :: Word8], alawEncode (alawDecode c) /= c ]

    , testCase "bit errors arrive at the rate asked for" $ do
        let n = 40000
            p = 0.01
            x = tone 700 0.5 n
            y = applyChannel fs idealChannel { chCodec = Just Ulaw, chBitError = Just p } x
            clean = applyChannel fs idealChannel { chCodec = Just Ulaw } x
            hit = length [ () | i <- [0 .. n - 1]
                         , VS.unsafeIndex y i /= VS.unsafeIndex clean i ]
            got = fromIntegral hit / fromIntegral n :: Double
        assertBool ("measured " ++ show got ++ ", wanted " ++ show p) (abs (got - p) < 0.2 * p)

    , testCase "loss arrives in bursts of the length asked for" $ do
        -- Gilbert-Elliott: the reason to have it rather than a coin flip
        -- per frame is that real loss comes in runs, and a modem rides
        -- out one lost frame and loses lock over five.
        let n = 400000
            q = 0.25          -- mean burst of four frames
            loss = Loss 0.02 0.02 q Silence
            x = VS.replicate n 0.5
            y = applyChannel fs idealChannel { chLoss = Just loss } x
            blk = 160
            lost = [ VS.unsafeIndex y (f * blk) == 0 | f <- [0 .. n `div` blk - 1] ]
            runs = [ length r | r <- group lost, head r ]
            mean = fromIntegral (sum runs) / fromIntegral (length runs) :: Double
        assertBool ("mean burst " ++ show mean ++ " frames over " ++ show (length runs) ++ " bursts")
          (abs (mean - 1 / q) < 0.5)

    , testCase "a lost frame is concealed the way it was asked to be" $ do
        -- 1234 Hz rather than a submultiple of the sample rate: a
        -- 1000 Hz tone at 8 kHz is exactly zero every fourth sample, and
        -- a test that counts zeros would be counting the sampling grid
        -- rather than the concealment.
        let x = VS.generate 16000 (\i -> 0.5 * sin (2 * pi * 1234 * fromIntegral i / fs))
            run how = applyChannel fs idealChannel { chLoss = Just (Loss 0.02 0.5 0.5 how) } x
            zeros y = length [ () | v <- VS.toList y, v == 0 ]
        assertBool ("silence leaves silence: " ++ show (zeros (run Silence)))
          (zeros (run Silence) > 1000)
        assertBool ("holding the last sample does not: " ++ show (zeros (run HoldLast)))
          (zeros (run HoldLast) < 50)
        assertBool ("nor does repeating the frame: " ++ show (zeros (run RepeatFrame)))
          (zeros (run RepeatFrame) < 50)
    ]

  , testGroup "transients"
    [ testCase "impulses arrive at the rate asked for" $ do
        let secs = 4
            n = round (fs * secs)
            lam = 50
            imp = Impulse lam 0.5 1500 0.002
            y = applyChannel fs idealChannel { chImpulse = Just imp } (VS.replicate n 0)
            -- one arrival is one excursion, so count crossings with a
            -- hold-off longer than the ring
            hold = round (0.006 * fs)
            arrivals = go 0 (negate hold) 0
            go i lastHit acc
              | i >= n = acc :: Int
              | abs (VS.unsafeIndex y i) > 0.25 && i - lastHit > hold = go (i + 1) i (acc + 1)
              | otherwise = go (i + 1) lastHit acc
            want = lam * secs
        assertBool ("counted " ++ show arrivals ++ ", expected about " ++ show want)
          (abs (fromIntegral arrivals - want) < 0.25 * want)

    , testCase "a dropout is a hit twenty decibels down" $ do
        -- Measured as power removed rather than as samples under a
        -- threshold: twenty hits a second lasting 10 ms each cover a
        -- fifth of the signal, and at -20 dB they leave a hundredth of
        -- the power there, so the whole signal should come out at
        -- 0.8 + 0.2/100 of its power -- about a decibel down.
        let x = tone 1234 0.5 240000
            y = applyChannel fs idealChannel { chHits = Just (Hits 20 0.01 (-20)) } x
            got = 20 * logBase 10 (rms y / rms x)
            want = 10 * logBase 10 (0.8 + 0.2 * 0.01)
        assertBool ("level " ++ show got ++ " dB, expected about " ++ show want)
          (abs (got - want) < 0.3)
    ]

  , testGroup "time and phase"
    [ testCase "wow moves the pitch by the percentage asked for" $ do
        -- The point of holding this in percent: one percent of speed is
        -- one percent of every frequency, so a 1800 Hz carrier swings
        -- +/- 18 Hz however slow the wow is.
        let dev = 0.01
            x = tone 1800 0.8 32000
            y = applyChannel fs idealChannel { chJitter = WowFlutter [(dev, 2)] } x
            hz = instantHz y
            mid = take (length hz - 200) (drop 200 hz)
        assertClose "fastest" 0.02 (1800 * (1 + dev)) (maximum mid)
        assertClose "slowest" 0.02 (1800 * (1 - dev)) (minimum mid)

    , testCase "phase jitter is the same angle at every frequency, and wow is not" $ do
        -- The distinction that stops one standing in for the other.  A
        -- delay modulation shifts a component in proportion to its
        -- frequency; carrier phase jitter shifts every component by the
        -- same angle.  Over V.32's 600 to 3000 Hz that is a factor of
        -- five, so measure the pitch swing at both ends of the band and
        -- see which impairment cares.
        -- Measured as phase rather than as frequency: mix the tone down
        -- against its own nominal carrier, low-pass, and read the angle.
        -- Counting zero crossings would not do here, because it needs
        -- samples per cycle to spare and the claim is about a band.
        let swingAt hz ch =
              let y = applyChannel fs ch (tone hz 0.8 32000)
                  (i0, q0) = mixDownAt (2 * pi * hz / fs) 0 y
                  lp = firCentered (firLowpass fs 100 201)
                  (i1, q1) = (lp i0, lp q0)
                  ph = [ atan2 (VS.unsafeIndex q1 k) (VS.unsafeIndex i1 k) | k <- [4000 .. 28000] ]
              in maximum ph - minimum ph
            -- a tenth of a percent, so the phase swing stays well inside
            -- a turn and needs no unwrapping
            wowCh = idealChannel { chJitter = WowFlutter [(0.001, 4)] }
            jitCh = idealChannel { chPhaseJitter = Just (20, 4) }
            ratio ch = swingAt 1200 ch / swingAt 400 ch
        assertClose "wow scales with frequency" 0.1 3 (ratio wowCh)
        assertClose "phase jitter does not" 0.1 1 (ratio jitCh)
        -- and the angle is the one that was asked for
        assertClose "20 degrees peak to peak" 0.1 (20 * pi / 180) (swingAt 1200 jitCh)

    , testCase "and the pitch swing does not depend on how fast the wow is" $ do
        -- The delay excursion goes as amplitude over rate -- one percent
        -- is 25 samples at 0.5 Hz and a fifth of a sample at 30 Hz --
        -- but the pitch swing is one percent either way.  A 600 Hz
        -- carrier, because the estimator counts zero crossings and needs
        -- samples per cycle to spare.
        let x = tone 600 0.8 64000
            hzOf f = let y = applyChannel fs idealChannel { chJitter = WowFlutter [(0.01, f)] } x
                     in take 2000 (drop 400 (instantHz y))
            swing f = maximum (hzOf f) - minimum (hzOf f)
        assertClose "1 percent at 2 Hz" 0.05 12 (swing 2)
        assertClose "and the same at 8 Hz" 0.08 (swing 2) (swing 8)
    ]

  , testGroup "ringing"
    [ testCase "a singing circuit rings down at its loop gain" $ do
        -- Round the loop k times is gain^k, which is what a comb from a
        -- feedback path does and what a single resonant pole pair does
        -- not.
        let g = 0.7
            d = 0.01
            n = 8000
            x = VS.generate n (\i -> if i == 0 then 1 else 0)
            y = applyChannel fs idealChannel { chSing = Just (d, g) } x
            step = round (d * fs)
            echoes = [ VS.unsafeIndex y (k * step) | k <- [0 .. 5] ]
        sequence_ [ assertClose ("echo " ++ show k) 1e-9 (g ^^ k) v
                  | (k, v) <- zip [0 :: Int ..] echoes ]

    , testCase "and cannot be asked to oscillate" $ do
        -- A loop gain of 1 is not a ringing circuit, it is one that
        -- never stops.  The gain is clamped below it.
        let x = VS.generate 8000 (\i -> if i == 0 then 1 else 0)
            y = applyChannel fs idealChannel { chSing = Just (0.01, 4) } x
        assertBool "stays bounded" (VS.maximum (VS.map abs y) < 2)
    ]

  , testCase "the same seed gives the same line, twice" $ do
      let ch = idealChannel { chSnrDb = Just 20, chImpulse = Just (Impulse 10 0.2 1200 0.002)
                            , chBitError = Just 0.001, chCodec = Just Ulaw }
          x = tone 1000 0.5 8000
      assertEqual "deterministic" (VS.toList (applyChannel fs ch x)) (VS.toList (applyChannel fs ch x))
  ]
  where
    assertClose what tol want got =
      assertBool (what ++ ": wanted " ++ show want ++ ", got " ++ show got)
                 (abs (got - want) <= tol * max 1 (abs want))
    group [] = []
    group (v : vs) = let (a, b) = span (== v) vs in (v : a) : group b
    -- Instantaneous frequency from positive-going zero crossings.
    instantHz y =
      let n = VS.length y
          crossings = [ fromIntegral i - v0 / (v1 - v0)
                      | i <- [1 .. n - 1]
                      , let v0 = VS.unsafeIndex y (i - 1), let v1 = VS.unsafeIndex y i
                      , v0 <= 0, v1 > 0 ]
      in zipWith (\a b -> fs / (b - a)) crossings (drop 1 crossings)
