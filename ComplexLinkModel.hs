{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module ComplexLinkModel where
import Data.Set as Set
import Palladio
import Security

import Misc


data OSI = Physical | DataLink | Network | Transport | Session | Presentation | Application
  deriving (Enum, Show, Bounded, Ord, Eq)


class (Ord (Location m),
       Ord (EncryptionScheme m),
       BasicDesignModel m) => ComplexLinkAccessModel m where
  data EncryptionScheme m

  linkAccessibleByAttackerWithAbilities :: LinkingResource m -> Set (TamperingMethod m) -> Bool
  encryptionAppliedOn                   :: LinkingResource m -> OSI -> Set (EncryptionScheme m)

  serviceCallDetectableFromLayer        :: LinkingResource m -> OSI
  
  schemesBreakableBy                    :: Attacker m -> Set (EncryptionScheme m)

  isNoisy                               :: LinkingResource m -> Bool



instance (ComplexLinkAccessModel m, ConcreteDesignModel m) => LinkAccessModel m where
  link `exposesPhsicallyAccessiblePayloadTo`  attacker =               -- Ein Angreifer kann auf einem link die Inhalte von Service-Calls sehen g.d.w:
    linkAccessibleByAttackerWithAbilities link (tamperingAbilities attacker) && -- Er auf auf das Medium Zugreifen kann, und ..
    (∃) (\osilayer -> (canAccessLayer osilayer) &&                              -- auf einem osilayer müssen Nutzdaten lesbar ...
        (∀) (\osilayer' ->  (osilayer < osilayer') → canAccessLayer osilayer') -- und nicht auf auf einem höheren osilayer' verborgen sein
    )
    where canAccessLayer = canAccess link attacker


  link `exposesPhsicallyAccessibleMetaDataTo` attacker =                -- Ein Angreifer kann auf einem link erkennen, dass Service-Calls gemacht werden, g.d.w:
    (not.isNoisy) link ||                                               -- Wenn auf dem Link nicht "viel los ist" (tm) (denn dann  haben wir eh verloren:
                                                                        -- hinreichend genaue statistische Methoden werden einem dann schon sagen, was los ist), oder falls
    linkAccessibleByAttackerWithAbilities link (tamperingAbilities attacker) && -- Er auf auf das Medium Zugreifen kann, und ..
    (∃) (\osilayer -> (serviceCallDetectableFromLayer link) <= osilayer &&      -- auf einem osilayer muss ein serviceCall aufgrund der Layer-Metadaten erkennbar ist, und ..
        (∀) (\osilayer' -> (osilayer' < osilayer) → canAccessLayer osilayer')  -- diese Metadaten dürfen nicht schon durch einen niedrigeren Layer verborgen sind
    )
    where canAccessLayer = canAccess link attacker

canAccess ::  (ComplexLinkAccessModel m) => (LinkingResource m) -> (Attacker m) -> OSI -> Bool
canAccess link attacker layer =
 isEmpty (encryptionAppliedOn link layer) ||
 (not.isEmpty) (encryptionAppliedOn link layer ∩ schemesBreakableBy attacker)



data DecryptionAbility     = NSA | Hacker | None  deriving (Ord, Enum, Eq, Bounded)
class (Ord (Location m),
       Ord (TamperingMethod m),
       ConcreteDesignModel m) => SimpleLinkProfileModel m where
  linkType               :: LinkingResource m -> LinkProfile
  decryptionAbilityOf    :: Attacker m -> DecryptionAbility

  hasPSK :: TamperingMethod m



data LinkProfile         = EthernetTLS | WPA_PSK | InternetTLS | Ethernet

instance (SimpleLinkProfileModel m) => ComplexLinkAccessModel m where
  data EncryptionScheme m = TLS_RSA | TLS_DH | TLS_DHE | TLS_ECDH | TLS_ECDHE | TLS_DH_anon | TLS_PSK
                          | TRIPLE_DES_CBC | AES_CBC | RC4 | WPA_TKIP | WPA_CCMP deriving (Ord, Show, Eq, Enum, Bounded)


  linkAccessibleByAttackerWithAbilities link abilities = mediumAccessibleIfPhysicallyReachable (linkType link)
    where mediumAccessibleIfPhysicallyReachable EthernetTLS = True
          mediumAccessibleIfPhysicallyReachable InternetTLS = True
          mediumAccessibleIfPhysicallyReachable Ethernet    = True
          mediumAccessibleIfPhysicallyReachable WPA_PSK     = hasPSK ∈ abilities


  encryptionAppliedOn link layer        = encryptionOn (linkType link) layer
    where encryptionOn EthernetTLS Transport = Set.fromList [TLS_RSA, TLS_DH, TLS_DHE, TLS_ECDH, TLS_ECDHE, TLS_DH_anon, TLS_PSK, TRIPLE_DES_CBC, AES_CBC, RC4]
          encryptionOn EthernetTLS _         = Set.fromList []

          encryptionOn WPA_PSK     Physical  = Set.fromList [RC4, WPA_TKIP]
          encryptionOn WPA_PSK     _         = Set.fromList []

          encryptionOn InternetTLS Transport = Set.fromList [TLS_RSA, AES_CBC]
          encryptionOn InternetTLS _         = Set.fromList []

          encryptionOn Ethernet    _         = Set.fromList []


  serviceCallDetectableFromLayer link   = detectableFromLayer (linkType link)
    where detectableFromLayer EthernetTLS = Physical
          detectableFromLayer WPA_PSK     = Network
          detectableFromLayer InternetTLS = Physical
  
  schemesBreakableBy attacker           = breakable (decryptionAbilityOf attacker)
    where breakable NSA    = Set.fromList allValues
          breakable Hacker = Set.fromList [RC4]

  isNoisy link                          = noisy (linkType link)
    where noisy EthernetTLS = False
          noisy WPA_PSK     = False
          noisy InternetTLS = True
          noisy Ethernet    = False


  
