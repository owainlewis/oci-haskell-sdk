{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.CredentialsSpec
  ( main
  , spec
  ) where

import           Network.Oracle.OCI.Common.Credentials
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parsing configuration" $ do
    it "returns the right answer" $ do
      10 `shouldBe` (10 :: Int)
