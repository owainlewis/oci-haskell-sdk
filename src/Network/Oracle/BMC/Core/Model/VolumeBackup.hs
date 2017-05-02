{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.VolumeBackup
  ( VolumeBackup(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data VolumeBackup = VolumeBackup
  {
  } deriving (Show)

instance FromJSON VolumeBackup where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
