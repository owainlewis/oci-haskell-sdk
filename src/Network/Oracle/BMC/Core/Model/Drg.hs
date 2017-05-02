{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Drg
  ( Drg(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Drg = Drg
  {
  } deriving (Show)

instance FromJSON Drg where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
