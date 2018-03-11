{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.SignatureSpec
  ( main
  , spec
  ) where

import qualified Network.HTTP.Client                         as H
import           Network.HTTP.Simple
import qualified Network.Oracle.OCI.Common.Signatures.Signer as S
import           Test.Hspec

listInstancesRequest :: H.Request
listInstancesRequest =
        setRequestHost "iaas.us-phoenix-1.oraclecloud.com"
      $ setRequestPath "/20160918/instances"
      $ setRequestSecure True
      $ setRequestPort 443
      $ setRequestQueryString [ ("availabilityDomain", Just "Pjwf%3A%20PHX-AD-1")
                              , ("compartmentId", Just compartment)
                              , ("displayName", Just "TeamXInstances")
                              , ("volumeId", Just "ocid1.volume.oc1.phx.abyhqljrgvttnlx73nmrwfaux7kcvzfs3s66izvxf2h4lgvyndsdsnoiwr5q")]
      $ defaultRequest
      where compartment = "compartmentId=ocid1.compartment.oc1..aaaaaaaam3we6vgnherjq5q2idnccdflvjsnog7mlr6rtdb25gilchfeyjxa"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Deriving request signatures" $ do
    it "returns the correct request signature" $ do
      1 `shouldBe` (1 :: Int)
