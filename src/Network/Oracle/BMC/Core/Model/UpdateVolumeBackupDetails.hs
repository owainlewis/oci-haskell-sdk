{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateVolumeBackupDetails
  ( UpdateVolumeBackupDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateVolumeBackupDetails = UpdateVolumeBackupDetails
  {
  } deriving (Show)

instance FromJSON UpdateVolumeBackupDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
