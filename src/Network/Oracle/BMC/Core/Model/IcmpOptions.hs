{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IcmpOptions
  ( IcmpOptions(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IcmpOptions = IcmpOptions
  {
  } deriving (Show)

instance FromJSON IcmpOptions where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
