{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.ComputeSpec
    ( main
    , spec
    ) where

import Network.Oracle.BMC.Core.Compute
import Network.Oracle.BMC.Core.Requests.ListInstancesRequest
import Network.Oracle.BMC.Credentials(defaultCredentialsProvider)

import Test.Hspec

main :: IO ()
main = hspec spec

cloudOCID = "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

spec :: Spec
spec = do
  describe "getInstances" $ do
    it "should return a list of instances" $ do
      instances <- listInstances defaultCredentialsProvider (listInstancesRequest cloudOCID)
      print instances
