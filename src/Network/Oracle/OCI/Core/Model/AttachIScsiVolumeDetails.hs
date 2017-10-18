{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.AttachIScsiVolumeDetails
  ( AttachIScsiVolumeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data AttachIScsiVolumeDetails = AttachIScsiVolumeDetails
  {
  } deriving (Show)

instance FromJSON AttachIScsiVolumeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
