{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UdpOptions
  ( UdpOptions(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UdpOptions = UdpOptions
  {
  } deriving (Show)

instance FromJSON UdpOptions where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
