{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.InstanceCredentials
  ( InstanceCredentials(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data InstanceCredentials = InstanceCredentials
  {
  } deriving (Show)

instance FromJSON InstanceCredentials where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
