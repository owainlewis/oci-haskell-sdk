{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IScsiVolumeAttachment
  ( IScsiVolumeAttachment(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IScsiVolumeAttachment = IScsiVolumeAttachment
  {
  } deriving (Show)

instance FromJSON IScsiVolumeAttachment where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
