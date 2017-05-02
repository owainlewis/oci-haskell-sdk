{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Subnet
  ( Subnet(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Subnet = Subnet
  {
  } deriving (Show)

instance FromJSON Subnet where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
