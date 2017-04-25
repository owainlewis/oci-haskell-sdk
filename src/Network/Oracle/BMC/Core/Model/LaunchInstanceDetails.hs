module Network.Oracle.BMC.Core.Model.LaunchInstanceDetals where

import Control.Monad (mzero)
import Data.Aeson

data LaunchIntanceDetails = LaunchInstanceDetails {
    availabilityDomain :: String
  , compartmentId :: String
  , displayName :: Maybe String
  , hostnameLabel :: Maybe String
  , imageId :: String
  , ipxeScript :: Maybe String
--  , metadata :: Maybe (String, String)
  , shape :: String
  , subnetId :: String
} deriving ( Show )
