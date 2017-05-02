{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Volume
  ( Volume(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Volume = Volume
  {
  } deriving (Show)

instance FromJSON Volume where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
