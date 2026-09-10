{-# LANGUAGE OverloadedStrings #-}
-- | The in-band framing a voice-mode modem uses, and the dialogue that
-- gets it streaming.
module Suite.Voice (voiceTests) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Modec.Sample (SampleFormat (..))
import Modec.Standards (Role (..))
import Modec.Voice

-- | Bytes with 0x10 far more often than chance would put it.
payloadGen :: Gen [Word8]
payloadGen = listOf (frequency [(3, pure 0x10), (1, pure 0x03), (8, arbitrary)])

-- | What a modem might send: samples, events and possibly an end.
data Token = Data [Word8] | Event Word8 deriving Show

tokenGen :: Gen Token
tokenGen = frequency
  [ (4, Data <$> payloadGen)
  , (1, Event <$> elements [0x62, 0x75, 0x52, 0x54, 0x31]) ]

render :: [Token] -> B.ByteString
render = B.concat . map one
  where
    one (Data ws) = dleEncode (B.pack ws)
    one (Event c) = B.pack [0x10, c]

-- | Decode in several pieces, gathering what each produced.
decodeIn :: [B.ByteString] -> (B.ByteString, [Word8], Maybe B.ByteString)
decodeIn = go dleInit B.empty []
  where
    go _ p evs [] = (p, evs, Nothing)
    go st p evs (b : bs) =
      let (st', d) = dleDecode st b
          p' = p <> dlPayload d
          evs' = evs ++ dlEvents d
      in case dlRest d of
           Just rest -> (p', evs', Just (rest <> B.concat bs))
           Nothing -> go st' p' evs' bs

voiceTests :: TestTree
voiceTests = testGroup "voice mode"
  [ testProperty "shielding round-trips whatever the samples are" $
      forAll payloadGen $ \ws ->
        decodeIn [dleEncode (B.pack ws)] === (B.pack ws, [], Nothing)

  , testProperty "the stream can be cut anywhere" $
      forAll (listOf tokenGen) $ \toks ->
        let wire = render toks
            whole = decodeIn [wire]
        in conjoin [ decodeIn [B.take i wire, B.drop i wire] === whole | i <- [0 .. B.length wire] ]

  , testProperty "events come out in order and leave the payload alone" $
      forAll (listOf tokenGen) $ \toks ->
        let (p, evs, end) = decodeIn [render toks]
        in p === B.concat [ B.pack ws | Data ws <- toks ]
           .&&. evs === [ c | Event c <- toks ]
           .&&. end === Nothing

  , testCase "an event inside two-byte samples keeps the byte parity" $ do
      let wire = B.pack [0x01, 0x02, 0x10, 0x75, 0x03, 0x04]   -- <DLE>u between two samples
          (p, evs, _) = decodeIn [wire]
      assertEqual "payload" (B.pack [1, 2, 3, 4]) p
      assertEqual "event" [0x75] evs
      assertEqual "even" 0 (B.length p `mod` 2)

  , testCase "<DLE><ETX> ends the stream and what follows is not samples" $ do
      let wire = B.pack [0x05, 0x06, 0x10, 0x03] <> "\r\nVCON\r\n"
      assertEqual "in one piece" (B.pack [5, 6], [], Just "\r\nVCON\r\n") (decodeIn [wire])
      assertEqual "cut between DLE and ETX" (B.pack [5, 6], [], Just "\r\nVCON\r\n")
        (decodeIn [B.take 3 wire, B.drop 3 wire])

  , testCase "a doubled DLE is one sample byte, at a block boundary too" $
      assertEqual "0x10" (B.pack [0x10], [], Nothing) (decodeIn [B.pack [0x10], B.pack [0x10]])

  , testCase "the set-up asks for the format and the answering side waits to ring" $ do
      let orig = voiceSetup 133 Originate
          ans = voiceSetup 133 Answer
      assertBool "+VSM" (Send "AT+VSM=133,8000" ["OK"] `elem` orig)
      assertBool "no wait when calling" (WaitFor "RING" `notElem` orig)
      let (before, fromRing) = break (== WaitFor "RING") ans
      assertBool "the answerer waits" (not (null fromRing))
      assertBool "off hook only after the ring" (all (/= Send "AT+VLS=1" ["OK", "VCON"]) before)
      assertEqual "the formats the modem has codes for"
        [Just 0, Just 1, Nothing, Just 131, Just 132, Just 133]
        (map vsmCode [S8, U8, S16, Ulaw, Alaw, Pcm14])

  , testCase "final results are recognised through their line ends" $ do
      assertEqual "OK" (Just "OK") (atFinal "\r\nOK\r\n")
      assertEqual "CONNECT with a rate" (Just "CONNECT") (atFinal "CONNECT 9600\r\n")
      assertEqual "echo is not a result" Nothing (atFinal "AT+FCLASS=8\r")
      assertEqual "nor is information text" Nothing (atFinal "+VSM: 133,8000")
      assertEqual "the ring" (Just "RING") (atFinal "RING\r\n")
  ]
