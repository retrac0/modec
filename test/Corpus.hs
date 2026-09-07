-- | Fixture recordings, and the tests that read them back.
module Corpus (fixtureDir, specFromName, fixtureTests) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (isSuffixOf, sort)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Modec.FSK
import Modec.Standards
import Modec.Wav

fixtureDir :: FilePath
fixtureDir = "test/fixtures"

-- | Fixture file names encode the standard and channel.
specFromName :: String -> FskSpec
specFromName name
  | "bell103_orig" `prefix` name = bell103Originate
  | "bell103_ans" `prefix` name = bell103Answer
  | "v21_ch1" `prefix` name = v21Channel1
  | "v21_ch2" `prefix` name = v21Channel2
  | otherwise = error ("cannot infer spec from fixture name " ++ name)
  where prefix p s = take (length p) s == p

fixtureTests :: IO TestTree
fixtureTests = do
  files <- sort . filter (".wav" `isSuffixOf`) <$> listDirectory fixtureDir
  return $ testGroup "minimodem fixtures" [ fixtureCase f | f <- files ]
  where
    fixtureCase f = testCase f $ do
      w <- readWav (fixtureDir </> f)
      expected <- B.readFile (fixtureDir </> replaceExtension f "txt")
      let spec = specFromName f
          fs = fromIntegral (wavRate w)
          got = B.pack (demodulate fs spec framing8N1 defaultDemodParams (wavSamples w))
      assertEqual "decoded bytes" (BC.unpack expected) (BC.unpack got)
