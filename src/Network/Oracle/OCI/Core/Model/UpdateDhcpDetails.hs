{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateDhcpDetails
  ( UpdateDhcpDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateDhcpDetails = UpdateDhcpDetails
  {
  } deriving (Show)

instance FromJSON UpdateDhcpDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
