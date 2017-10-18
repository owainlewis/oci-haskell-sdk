{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.VnicAttachment
  ( VnicAttachment(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data VnicAttachment = VnicAttachment
  {
  } deriving (Show)

instance FromJSON VnicAttachment where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
