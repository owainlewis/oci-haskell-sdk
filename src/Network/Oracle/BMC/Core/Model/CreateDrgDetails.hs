{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateDrgDetails
  ( CreateDrgDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateDrgDetails = CreateDrgDetails
  {
  } deriving (Show)

instance FromJSON CreateDrgDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
