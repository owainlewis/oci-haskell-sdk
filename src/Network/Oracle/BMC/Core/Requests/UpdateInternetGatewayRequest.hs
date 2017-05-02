{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateInternetGatewayRequest where

data UpdateInternetGatewayRequest = UpdateInternetGatewayRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateInternetGatewayRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
