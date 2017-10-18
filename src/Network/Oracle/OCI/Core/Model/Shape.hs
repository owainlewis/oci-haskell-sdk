{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Shape
  ( Shape(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Shape = Shape
  {
  } deriving (Show)

instance FromJSON Shape where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
