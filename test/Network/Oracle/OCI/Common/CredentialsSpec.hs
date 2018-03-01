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
  describe "parseValidConfig" $ do
    it "should return a credentials object" $ do
      "" `shouldEqual` ""
