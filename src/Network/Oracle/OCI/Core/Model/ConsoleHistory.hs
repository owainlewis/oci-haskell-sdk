{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.ConsoleHistory
  ( ConsoleHistory(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data ConsoleHistory = ConsoleHistory
  {
  } deriving (Show)

instance FromJSON ConsoleHistory where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
