{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.DhcpDnsOption
  ( DhcpDnsOption(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data DhcpDnsOption = DhcpDnsOption
  {
  } deriving (Show)

instance FromJSON DhcpDnsOption where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
