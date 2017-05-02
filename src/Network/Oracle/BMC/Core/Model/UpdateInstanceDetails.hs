{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateInstanceDetails
  ( UpdateInstanceDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateInstanceDetails = UpdateInstanceDetails
  {
  } deriving (Show)

instance FromJSON UpdateInstanceDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
