{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.AttachVolumeDetails
  ( AttachVolumeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data AttachVolumeDetails = AttachVolumeDetails
  {
  } deriving (Show)

instance FromJSON AttachVolumeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
