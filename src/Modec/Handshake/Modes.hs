-- | Which modulations this modem will negotiate, in what order, and what
-- that implies.
--
-- All of it is a function of the configured mode list and which rungs of
-- the ladder have already been tried -- no audio, no clock, no phase.
-- It was forty lines of @where@ bindings in the middle of
-- 'Modec.Handshake.handshakeStep', mixed in among the tone detectors, so
-- "what are we willing to run" and "what is on the line" could not be
-- read or tested apart.
module Modec.Handshake.Modes
  ( Ladder (..)
  , ladder
  , probesFor
  , fskTx
  , fskRx
  ) where

import Data.Maybe (listToMaybe)

import Modec.Link
import Modec.Standards

-- | What the mode list permits.
data Ladder = Ladder
  { laModes      :: [Standard]              -- ^ as configured, best first
  , laAllows     :: Standard -> Bool
  , laV22        :: !Bool                   -- ^ any of the V.22 family
  , la2400       :: !Bool                   -- ^ V.22bis, so S1 is worth sending
  , laItu        :: !Bool                   -- ^ any mode that opens with the ITU answer tone
  , laV23        :: !Bool
  , laBellDpsk   :: [Standard]
    -- ^ modes that answer our 2225 Hz with scrambled DPSK marks.  V.22
    -- modems do this as well as Bell 212A ones (V.22 §6.3.1.1 note), so
    -- either name is accepted.
  , laFskOnly    :: !Bool
    -- ^ nothing in the V.22 family is configured, so there is nothing to
    -- disturb by transmitting early: follow V.25 and put the carrier up
    -- as the answer tone ends rather than waiting to hear the
    -- answerer's.  An answering modem stepping a ladder may hold each
    -- rung open for only a second or two.
  , laPreferred  :: Standard                -- ^ the FSK mode to reach for first
  , laBellChoice :: Maybe Standard          -- ^ what a 2225 Hz answer tone is answered with
  , laOfferV32   :: !Bool                   -- ^ this answerer may offer V.32 on spec
  , laProbes     :: [Standard]              -- ^ the answering rotation
  , laRotating   :: !Bool                   -- ^ more than one rung to rotate through
  , laNext       :: Standard -> Standard    -- ^ the rung after this one, wrapping
  , laFirst      :: Standard
  }

-- | Read the ladder off the configuration and what has been tried.
--
-- @tried212@ is a Bell 212A attempt that already failed, after which
-- Bell 103 is preferred; @triedV32@ is a V.32 offer that nothing took
-- up, which is not repeated -- an answering modem offers V.32 once and
-- does not come back to it.
ladder :: [Standard] -> Role -> Bool -> Bool -> Ladder
ladder modes role tried212 triedV32 = Ladder
  { laModes = modes
  , laAllows = allowed
  , laV22 = v22Allowed
  , la2400 = V22bis `elem` modes
  -- V.23 belongs here too: its answerer opens with the ITU answer tone
  -- like any other ITU mode, and only a Bell-only modem skips it.
  , laItu = any (`elem` modes) [V21, V22, V22bis, V23]
  , laV23 = v23Allowed
  , laBellDpsk = [ s | s <- [Bell212A, V22], allowed s ]
  , laFskOnly = not v22Allowed && (allowed V21 || allowed Bell103 || v23Allowed)
  , laPreferred = case [ s | s <- modes, s `elem` [V21, Bell103, V23] ] of
      (s : _) -> s
      []      -> V21
  , laBellChoice = listToMaybe
      [ s | s <- modes, s `elem` [Bell212A, Bell103], not (s == Bell212A && tried212) ]
  -- Nothing in V.32 says to fall back to another modulation at all --
  -- Note 5 permits only disconnecting, and not within 3 s of the pair --
  -- but this modem answers for V.21 and Bell 103 too, which are outside
  -- V.32's scope entirely.  It stays on the line, so that floor does not
  -- bind.
  , laOfferV32 = role == Answer && any isV32 modes && not triedV32
  , laProbes = order
  , laRotating = length order > 1
  , laNext = \s -> case dropWhile (/= s) order of
      (_ : n : _) -> n
      _           -> head order
  , laFirst = case order of
      (p : _) -> p
      []      -> Bell103
  }
  where
    allowed s = s `elem` modes
    v22Allowed = any (`elem` modes) [V22, V22bis]
    v23Allowed = allowed V23
    order = probesFor modes

-- | One probe per family, in the traditional order, skipping families
-- this modem is not configured for.  V.32 is not among them: it is
-- offered once from inside the V.22 rung rather than taking a turn in
-- the rotation.
probesFor :: [Standard] -> [Standard]
probesFor modes =
  [ p | (p, needed) <- [ (V22, any (`elem` modes) [V22, V22bis])
                       , (V21, V21 `elem` modes)
                       , (V23, V23 `elem` modes)
                       , (Bell103, Bell103 `elem` modes || Bell212A `elem` modes) ]
      , needed ]

-- | The two channels of an FSK mode, from this end.  Both are partial by
-- construction: only an 'FskLink' has channels, and only the FSK modes
-- make one.
fskTx, fskRx :: Role -> Standard -> FskSpec
fskTx role s = case linkFor role s of
  FskLink t _ -> t
  V22Link {}  -> error "fskTx: V.22"
  V32Link {}  -> error "fskTx: V.32"
fskRx role s = case linkFor role s of
  FskLink _ r -> r
  V22Link {}  -> error "fskRx: V.22"
  V32Link {}  -> error "fskRx: V.32"
