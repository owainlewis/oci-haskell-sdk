{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateVcnRequest where

data UpdateVcnRequest = UpdateVcnRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateVcnRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
