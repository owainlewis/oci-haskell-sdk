-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMCS.Credentials
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- This module defined readers and types for Oracle BMCS credentials
-----------------------------------------------------------------------------
module Network.Oracle.BMCS.Credentials
    ( Credentials(..)
    ) where

-- [DEFAULT]
-- user=ocid1.user.oc1..aaaaaaaat5nvwcna5j6aqzjcmdy5eqbb6qt2jvpkanghtgdaqedqw3rynjq
-- fingerprint=20:3b:97:13:55:1c:5b:0d:d3:37:d8:50:4e:c5:3a:34
-- key_file=~/.oraclebmc/bmcs_api_key.pem
-- tenancy=ocid1.tenancy.oc1..aaaaaaaaba3pv6wkcr4jqae5f15p2bcmdyt2j6rx32uzr4h25vqstifsfdsq
--
data Credentials = Credentials {
    user :: String
  , fingerprint :: String
  , keyFile :: String
  , tenancy :: String
} deriving ( Eq, Show )
