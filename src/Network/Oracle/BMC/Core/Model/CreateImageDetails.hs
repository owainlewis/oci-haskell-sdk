{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateImageDetails
  ( CreateImageDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateImageDetails = CreateImageDetails
  {
  } deriving (Show)

instance FromJSON CreateImageDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
