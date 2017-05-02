{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateInternetGatewayDetails
  ( CreateInternetGatewayDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateInternetGatewayDetails = CreateInternetGatewayDetails
  {
  } deriving (Show)

instance FromJSON CreateInternetGatewayDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
