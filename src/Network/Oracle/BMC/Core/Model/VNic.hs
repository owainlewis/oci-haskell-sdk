module Network.Oracle.BMC.Core.Model.VNic were

-- | A virtual network interface card.
--   Each instance automatically has a VNIC attached to it,
--   and the VNIC connects the instance to the subnet.
data VNic = VNic where {
    availabilityDomain :: String
  , compartmentId :: String
  , displayName :: String
  , hostnameLabel :: String
  , id :: String
--  , lifecycleState
  , privateIp :: String
  , publicIp :: String
  , subnetId :: String
  , timeCreated :: String
} deriving ( Show )
