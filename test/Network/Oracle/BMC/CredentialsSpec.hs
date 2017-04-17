{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.CredentialsSpec
  ( main
  , spec
  ) where

import Network.Oracle.BMC.Credentials
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "configFileCredentialsProvider" $ do
          it "should load valid credentials from file" $ do
              credentials <- configFileCredentialsProvider "test/Fixtures/config" "DEFAULT"
              (user credentials) `shouldBe` "user"
              (fingerprint credentials) `shouldBe` "fingerprint"
              (tenancy credentials) `shouldBe` "tenancy"
              (region credentials) `shouldBe` "us-phoenix-1"
