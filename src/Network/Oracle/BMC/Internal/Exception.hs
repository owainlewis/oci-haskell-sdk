{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMC.Exception
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Unified exception types for HTTP request to Oracle Bare Metal Cloud
--
-----------------------------------------------------------------------------
module Network.Oracle.BMC.Internal.Exception
  ( BMCException(..)
  , throwLeftIO
  ) where

import Control.Exception (throwIO, Exception)
import Data.Typeable

--------------------------------------------------------
data BMCException
  = RSASignatureException String
  | InvalidCredentialsException String
  | GenericException String
  | JSONParseException
  deriving (Eq, Typeable)

instance Show BMCException where
  show (RSASignatureException e) = e
  show (InvalidCredentialsException e) = e
  show (GenericException e) = e
  show (JSONParseException) = "Unable to parse JSON"

instance Exception BMCException

------------------------------------------------------
throwLeftIO
  :: Exception e
  => IO (Either e b) -> IO b
throwLeftIO ioe = do
  result <- ioe
  case result of
    Left e -> throwIO e
    Right a -> return a
