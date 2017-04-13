{-# LANGUAGE DeriveDataTypeable #-}

module Network.Oracle.BMC.Exception
  ( BMCException(..)
  , throwLeftIO
  ) where

import Control.Exception
import Data.Typeable

--------------------------------------------------------
data BMCException
  = RSASignatureException String
  | InvalidCredentialsException String
  | GenericException String
  deriving (Eq, Typeable)

instance Show BMCException where
  show (RSASignatureException e) = e
  show (InvalidCredentialsException e) = e
  show (GenericException e) = e

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
