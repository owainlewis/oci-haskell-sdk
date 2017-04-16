module Network.Oracle.BMC.Model.Region
  ( Region(..)
  , computeRegionEndpoint
  ) where

import Data.Semigroup ((<>))

data Region =
  USPhoenix1

instance Show Region where
  show USPhoenix1 = "us-phoenix-1"

computeRegionEndpoint :: Region -> String
computeRegionEndpoint region = "iaas." <> show region <> ".oraclecloud.com"
