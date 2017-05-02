{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IPSecConnectionDeviceStatus
  ( IPSecConnectionDeviceStatus(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IPSecConnectionDeviceStatus = IPSecConnectionDeviceStatus
  {
  } deriving (Show)

instance FromJSON IPSecConnectionDeviceStatus where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
