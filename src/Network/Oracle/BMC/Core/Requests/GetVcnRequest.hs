{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetVcnRequest where

data GetVcnRequest = GetVcnRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetVcnRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
