{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateDrgAttachmentRequest where

data UpdateDrgAttachmentRequest = UpdateDrgAttachmentRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateDrgAttachmentRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
