{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Image
  ( Image(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data LifecycleState
  = Provisioning
  | Available
  | Disabled
  | Deleted

instance Show LifecycleState where
  show (Provisioning) = "PROVISIONING"
  show (Available) = "AVAILABLE"
  show (Disabled) = "DISABLED"
  show (Deleted) = "DELETED"

data Image = Image
  { baseImageId :: Maybe String
  , compartmentId :: String
  , createImageAllowed :: Bool
  , displayName :: Maybe String
  , id :: String
  -- todo move back to type level
  , lifecycleState :: String
  , operatingSystem :: String
  , operatingSystemVersion :: String
  , timeCreated :: String
  } deriving (Show)

instance FromJSON Image where
  parseJSON (Object v) =
    Image <$> v .: "baseImageId" <*> v .: "compartmentId" <*>
    v .: "createImageAllowed" <*>
    v .: "displayName" <*>
    v .: "id" <*>
    v .: "lifecycleState" <*>
    v .: "operatingSystem" <*>
    v .: "operatingSystemVersion" <*>
    v .: "timeCreated"
  parseJSON _ = mzero
