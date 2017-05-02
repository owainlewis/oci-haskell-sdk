{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteDrgAttachmentRequest where

data DeleteDrgAttachmentRequest = DeleteDrgAttachmentRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteDrgAttachmentRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
