module Network.Oracle.BMC.Core.Instance where

import Network.Oracle.BMC.Transport.Request

-- "https://iaas.us-phoenix-1.oraclecloud.com/20160918/instances
-- ?availabilityDomain=Pjwf%3A%20PHX-AD-1
-- &compartmentId=ocid1.compartment.
-- &displayName=TeamXInstances
-- &volumeId=ocid1.volume.oc1....

base = "https://iaas.us-phoenix-1.oraclecloud.com"

listInstances compartmentId =
    let req = HttpRequest { httpMethod = GET
                          , url = concat [base
                                         , "/20160918/instances?compartmentId="
                                         , compartmentId
                                         ]
                          , headers = []
                          , body = Nothing
                          } in
    runHttpsRequest req
