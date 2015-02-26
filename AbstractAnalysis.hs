{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}
module AbstractAnalysis where
import Data.Set.Monad as Set
import Palladio
import Security

import Misc
import Reasons
import ReasonsModel

import Control.Monad.Trans.Class(lift)


{- ... unter Verwendung folgender "Hilfsbegriffe" ... -}
accessibleParameters :: (AbstractDesignModel m, Reasons m) => (Attacker m) -> WithReason m (Parameter m)
accessibleParameters attacker =
  -- Ausgabe-Parameter, auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat
  [ parameter | interface <- interfacesAllowedToBeUsedByM attacker,
                interface ∈  systemProvides,
                service   <- lift $ services interface,
                parameter <- lift $ outputParameters service
  ] ⊔

  -- Eingabe-Parameter,auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat, weil er vom System Aufgerufen wird.
  [ parameter | interface <- interfacesAllowedToBeUsedByM attacker,
                interface ∈  systemRequires,
                service   <- lift $ services interface,
                parameter <- lift $ inputParameters service
  ] ⊔

  -- Parameter, auf die der Angreifer Zugriff hat, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ parameter | container <- containersFullyAccessibleBy attacker,
                interface <- (providedInterfacesOnM container) ⊔ (requiredInterfacesOnM container),
                service   <- lift $ services interface,
                parameter <- lift $ (inputParameters service) ∪ (outputParameters service),
                _ <- because [Inferred2 ContainerFullyAccessible attacker container]
  ] ⊔

  -- Parameter, auf die der Angreifer Zugriff hat, weil er eine entsprechende LinkResource angreifen konnte.
  [ parameter | link                   <- linksPayloadFullyAccessibleBy attacker,
                let (containerLeft,
                     containerRight)   =  linkBetween link,
                left                   <- lift $ assembliesOn containerLeft,
                interface              <- lift $ requires (componentOf left),
                let (ByAssembly right) =  systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                service                <- lift $ services interface,
                parameter              <- lift $ (inputParameters service) ∪ (outputParameters service),
                _ <- because [Inferred2 LinksPayloadFullyAccessibleBy attacker link]
  ]


observableServices :: (AbstractDesignModel m, Reasons m) => (Attacker m) -> WithReason m (Service m)
observableServices attacker =
  -- Services, deren Aufrufe der Angreifer als regulärer "Benutzer" des Systems beobachten kann
  [ service | interface <- interfacesAllowedToBeUsedByM attacker,
              interface ∈  systemProvides,
              service   <- lift $ services interface
  ] ⊔
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ service | container <- containersFullyAccessibleBy attacker,
              interface <- (providedInterfacesOnM container) ⊔ (requiredInterfacesOnM container),
              service   <- lift $ services interface,
              _ <- because [Inferred2 ContainerFullyAccessible attacker container]
  ] ⊔
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er eine entsprechende LinkResource angreifen konnte.
  [ service   | link                   <- linksMetaDataFullyAccessibleBy attacker,
                let (containerLeft,
                     containerRight)   =  linkBetween link,
                left                   <- lift $ assembliesOn containerLeft,
                interface              <- lift $ requires (componentOf left),
                let (ByAssembly right) = systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                service                <- lift $ services interface,
                _ <- because [Inferred2 LinksMetaDataFullyAccessibleBy attacker link]
  ]





{- Denn damit kann man direkt ein Analyserusultat bestimmen: -}
instance (AbstractDesignModel m, Reasons m) => AnalysisResult m where
  dataAccessibleTo attacker =
    [ dataSet  | parameter <- accessibleParameters attacker, dataSet <- classificationOfM parameter] ⊔
    [ dataSet  | service   <- observableServices attacker,   dataSet <- classificationOfCallM service]


classificationOfM :: (BasicDesignModel m, Reasons m) => Parameter m -> WithReason m (DataSet m)
classificationOfM = liftA2 ClassificationOf classificationOf

classificationOfCallM :: (BasicDesignModel m, Reasons m) => Service m -> WithReason m (DataSet m)
classificationOfCallM = liftA2 ClassificationOfCall classificationOfCall

interfacesAllowedToBeUsedByM :: (BasicDesignModel m, Reasons m) => Attacker m -> WithReason m (Interface m)
interfacesAllowedToBeUsedByM = liftA2 InterfacesAllowedToBeUsedBy interfacesAllowedToBeUsedBy


providedInterfacesOnM :: (PalladioComponentModel m, Reasons m) => ResourceContainer m -> WithReason m (Interface m)
providedInterfacesOnM = liftI2 ProvidedInterfacesOn providedInterfacesOn


requiredInterfacesOnM :: (PalladioComponentModel m, Reasons m) => ResourceContainer m -> WithReason m (Interface m)
requiredInterfacesOnM = liftI2 RequiredInterfacesOn requiredInterfacesOn

{-
linksMetaDataFullyAccessibleByM :: (AbstractDesignModel m, Reasons m) => Attacker m -> WithReason m (LinkingResource m)
linksMetaDataFullyAccessibleByM = liftI2 LinksMetaDataFullyAccessibleBy linksMetaDataFullyAccessibleBy


linksPayloadFullyAccessibleByM :: (AbstractDesignModel m, Reasons m) => Attacker m -> WithReason m (LinkingResource m)
linksPayloadFullyAccessibleByM = liftI2 LinksPayloadFullyAccessibleBy linksPayloadFullyAccessibleBy
-}
