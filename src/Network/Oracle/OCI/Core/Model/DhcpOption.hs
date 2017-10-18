{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.DhcpOption
  ( DhcpOption(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data DhcpOption = DhcpOption
  {
  } deriving (Show)

instance FromJSON DhcpOption where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
