{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.PortRange
  ( PortRange(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data PortRange = PortRange
  {
  } deriving (Show)

instance FromJSON PortRange where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
