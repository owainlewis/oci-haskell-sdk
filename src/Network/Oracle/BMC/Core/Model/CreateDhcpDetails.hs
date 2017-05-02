{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateDhcpDetails
  ( CreateDhcpDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateDhcpDetails = CreateDhcpDetails
  {
  } deriving (Show)

instance FromJSON CreateDhcpDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
