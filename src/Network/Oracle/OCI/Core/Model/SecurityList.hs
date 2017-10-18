{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.SecurityList
  ( SecurityList(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data SecurityList = SecurityList
  {
  } deriving (Show)

instance FromJSON SecurityList where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
