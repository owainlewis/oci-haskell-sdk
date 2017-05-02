{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateVolumeBackupDetails
  ( CreateVolumeBackupDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateVolumeBackupDetails = CreateVolumeBackupDetails
  {
  } deriving (Show)

instance FromJSON CreateVolumeBackupDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
