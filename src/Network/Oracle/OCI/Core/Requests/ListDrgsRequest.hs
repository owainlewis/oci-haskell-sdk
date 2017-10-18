{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListDrgsRequest where

data ListDrgsRequest = ListDrgsRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListDrgsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
