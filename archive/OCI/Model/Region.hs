module Network.Oracle.OCI.Model.Region
  ( Region(..)
  , computeRegionEndpoint
  ) where

data Region =
    USPhoenix1
  | USAshburn1
  | EUFrankfurt1

instance Show Region where
  show USPhoenix1   = "us-phoenix-1"
  show USAshburn1   = "us-ashburn-1"
  show EUFrankfurt1 = "eu-frankfurt-1"

computeRegionEndpoint :: Region -> String
computeRegionEndpoint region = "iaas." ++ (show region) ++ ".oraclecloud.com"
