{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMC.Internal.Exception
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Unified exception types for HTTP request to Oracle Bare Metal Cloud
--
-----------------------------------------------------------------------------
module Network.Oracle.OCI.Internal.Exception
  ( OCIException(..)
  , throwLeftIO
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Typeable

--------------------------------------------------------
data OCIException
  = RSASignatureException String
  | InvalidCredentialsException String
  | GenericException String
  | JSONParseException
  deriving (Eq, Typeable)

instance Show OCIException where
  show (RSASignatureException e)       = e
  show (InvalidCredentialsException e) = e
  show (GenericException e)            = e
  show (JSONParseException)            = "Unable to parse JSON"

instance Exception OCIException

------------------------------------------------------
throwLeftIO
  :: Exception e
  => IO (Either e b) -> IO b
throwLeftIO ioe = do
  result <- ioe
  case result of
    Left e  -> throwIO e
    Right a -> return a
