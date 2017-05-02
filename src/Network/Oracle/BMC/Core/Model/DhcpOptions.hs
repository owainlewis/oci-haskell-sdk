{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.DhcpOptions
  ( DhcpOptions(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data DhcpOptions = DhcpOptions
  {
  } deriving (Show)

instance FromJSON DhcpOptions where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
