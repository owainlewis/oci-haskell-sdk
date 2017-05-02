{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.DhcpSearchDomainOption
  ( DhcpSearchDomainOption(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data DhcpSearchDomainOption = DhcpSearchDomainOption
  {
  } deriving (Show)

instance FromJSON DhcpSearchDomainOption where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
