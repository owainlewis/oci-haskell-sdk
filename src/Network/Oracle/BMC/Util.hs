{-# LANGUAGE DeriveDataTypeable #-}
module Network.Oracle.BMC.Util where

import Control.Exception
import Data.Typeable

data OracleBMCException = GenericException String
  deriving (Typeable)

instance Show OracleBMCException where
  show (GenericException e) = e

instance Exception OracleBMCException

-- | Convert Either errors into impure IO errors
throwLeftIO :: Show e => IO (Either e b) -> IO b
throwLeftIO ioe = do
  result <- ioe
  case result of
    Left e -> throwIO (GenericException . show $ e)
    Right a -> return a
