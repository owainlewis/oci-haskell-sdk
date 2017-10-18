{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateRouteTableRequest where

data CreateRouteTableRequest = CreateRouteTableRequest
  {
  } deriving (Eq, Show)

instance ToRequest CreateRouteTableRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
