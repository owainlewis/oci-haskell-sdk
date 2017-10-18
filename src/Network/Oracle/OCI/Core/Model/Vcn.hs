{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Vcn
  ( Vcn(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Vcn = Vcn
  {
  } deriving (Show)

instance FromJSON Vcn where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
