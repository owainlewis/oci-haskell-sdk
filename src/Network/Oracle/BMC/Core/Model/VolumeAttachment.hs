{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.VolumeAttachment
  ( VolumeAttachment(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data VolumeAttachment = VolumeAttachment
  {
  } deriving (Show)

instance FromJSON VolumeAttachment where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
