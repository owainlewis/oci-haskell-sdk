{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IPSecConnection
  ( IPSecConnection(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IPSecConnection = IPSecConnection
  {
  } deriving (Show)

instance FromJSON IPSecConnection where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
