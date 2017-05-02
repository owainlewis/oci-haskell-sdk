{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateRouteTableDetails
  ( UpdateRouteTableDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateRouteTableDetails = UpdateRouteTableDetails
  {
  } deriving (Show)

instance FromJSON UpdateRouteTableDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
