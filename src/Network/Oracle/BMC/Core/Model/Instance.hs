{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Instance
  ( Instance(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson
data Instance = Instance
  { availabilityDomain :: String
  , compartmentId :: String
  , displayName :: String
  , id :: String
  , imageId :: String
  , region :: String
  , shape :: String
  , timeCreated :: String
  } deriving (Show)

instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$>
    v .: "availabilityDomain" <*>
    v .: "compartmentId" <*>
    v .: "displayName" <*>
    v .: "id" <*>
    v .: "imageId" <*>
    v .: "region" <*>
    v .: "shape" <*>
    v .: "timeCreated"
  parseJSON _ = mzero
