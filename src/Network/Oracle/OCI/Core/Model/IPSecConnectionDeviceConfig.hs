{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IPSecConnectionDeviceConfig
  ( IPSecConnectionDeviceConfig(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IPSecConnectionDeviceConfig = IPSecConnectionDeviceConfig
  {
  } deriving (Show)

instance FromJSON IPSecConnectionDeviceConfig where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
