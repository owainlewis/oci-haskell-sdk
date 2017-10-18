{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateCpeDetails
  ( CreateCpeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateCpeDetails = CreateCpeDetails
  {
  } deriving (Show)

instance FromJSON CreateCpeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
