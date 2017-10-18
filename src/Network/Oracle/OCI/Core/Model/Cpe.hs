{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Cpe
  ( Cpe(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Cpe = Cpe
  {
  } deriving (Show)

instance FromJSON Cpe where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
