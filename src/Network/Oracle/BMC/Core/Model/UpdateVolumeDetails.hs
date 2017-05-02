{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateVolumeDetails
  ( UpdateVolumeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateVolumeDetails = UpdateVolumeDetails
  {
  } deriving (Show)

instance FromJSON UpdateVolumeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
