{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Transport.SignedRequest where
-- ----------------------------------------------------------------------
-- mkBaseRequest :: Request
-- mkBaseRequest =
--     setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
--     setRequestSecure True $
--     setRequestPort 443 $
--     defaultRequest
-- ----------------------------------------------------------------------
-- listInstances compartmentId =
--     setRequestPath "/20160918/instances" $
--     setRequestQueryString [("compartmentId", Just compartmentId)]
--     mkBaseRequest
-- example = listInstances "ocid.123"
-- exampleHeaders = addGenericHeaders example
