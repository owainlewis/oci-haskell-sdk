{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetInternetGatewayRequest where

data GetInternetGatewayRequest = GetInternetGatewayRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetInternetGatewayRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
