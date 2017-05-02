{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListInternetGatewaysRequest where

data ListInternetGatewaysRequest = ListInternetGatewaysRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListInternetGatewaysRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
