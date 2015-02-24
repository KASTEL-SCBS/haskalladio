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
accessibleParameters :: (AbstractDesignModel m) => (Attacker m) -> Set (Parameter m)
accessibleParameters attacker =
  -- Ausgabe-Parameter, auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat
  [ parameter | interface <- interfacesAllowedToBeUsedBy attacker,
                interface ∈  systemProvides,
                service   <- services interface,
                parameter <- outputParameters service
  ] ∪

  -- Eingabe-Parameter,auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat, weil er vom System Aufgerufen wird.
  [ parameter | interface <- interfacesAllowedToBeUsedBy attacker,
                interface ∈  systemRequires,
                service   <- services interface,
                parameter <- inputParameters service
  ] ∪

  -- Parameter, auf die der Angreifer Zugriff hat, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ parameter | container <- containersFullyAccessibleBy attacker,
                interface <- (providedInterfacesOn container) ∪ (requiredInterfacesOn container),
                service   <- services interface,
                parameter <- (inputParameters service) ∪ (outputParameters service)
  ] ∪

  -- Parameter, auf die der Angreifer Zugriff hat, weil er eine entsprechende LinkResource angreifen konnte.
  [ parameter | link                   <- linksPayloadFullyAccessibleBy attacker,
                let (containerLeft,
                     containerRight)   =  linkBetween link,
                left                   <- assembliesOn containerLeft,
                interface              <- requires (componentOf left),
                let (ByAssembly right) =  systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                service                <- services interface,
                parameter              <- (inputParameters service) ∪ (outputParameters service)
  ]


observableServices :: (AbstractDesignModel m) => (Attacker m) -> Set (Service m)
observableServices attacker =
  -- Services, deren Aufrufe der Angreifer als regulärer "Benutzer" des Systems beobachten kann
  [ service | interface <- interfacesAllowedToBeUsedBy attacker,
              interface ∈  systemProvides,
              service   <- services interface
  ] ∪
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ service | container <- containersFullyAccessibleBy attacker,
              interface <- (providedInterfacesOn container) ∪ (requiredInterfacesOn container),
              service   <- services interface
  ] ∪
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er eine entsprechende LinkResource angreifen konnte.
  [ service   | link                   <- linksMetaDataFullyAccessibleBy attacker,
                let (containerLeft,
                     containerRight)   =  linkBetween link,
                left                   <- assembliesOn containerLeft,
                interface              <- requires (componentOf left),
                let (ByAssembly right) = systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                service                <- services interface
  ]





{- Denn damit kann man direkt ein Analyserusultat bestimmen: -}
instance (AbstractDesignModel m, Reasons m) => AnalysisResult m where
  dataAccessibleTo attacker =
    [ dataSet  | parameter <- lift $ accessibleParameters attacker, dataSet <- classificationOfM parameter] ⊔
    [ dataSet  | service   <- lift $ observableServices attacker,   dataSet <- classificationOfCallM service]


classificationOfM :: (BasicDesignModel m, Reasons m) => Parameter m -> WithReason m (DataSet m)
classificationOfM = liftR2 ClassificationOf classificationOf

classificationOfCallM :: (BasicDesignModel m, Reasons m) => Service m -> WithReason m (DataSet m)
classificationOfCallM = liftR2 ClassificationOfCall classificationOfCall
