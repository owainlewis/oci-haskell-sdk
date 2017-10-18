{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateVcnDetails
  ( UpdateVcnDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateVcnDetails = UpdateVcnDetails
  {
  } deriving (Show)

instance FromJSON UpdateVcnDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
