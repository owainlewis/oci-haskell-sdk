{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateInternetGatewayDetails
  ( UpdateInternetGatewayDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateInternetGatewayDetails = UpdateInternetGatewayDetails
  {
  } deriving (Show)

instance FromJSON UpdateInternetGatewayDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
