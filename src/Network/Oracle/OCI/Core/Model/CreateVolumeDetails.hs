{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateVolumeDetails
  ( CreateVolumeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateVolumeDetails = CreateVolumeDetails
  {
  } deriving (Show)

instance FromJSON CreateVolumeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
