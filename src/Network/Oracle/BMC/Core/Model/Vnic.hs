{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.Vnic
  ( Vnic(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data Vnic = Vnic
  {
  } deriving (Show)

instance FromJSON Vnic where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
