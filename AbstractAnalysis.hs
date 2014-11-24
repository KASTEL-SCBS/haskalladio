{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module AbstractAnalysis where
import Data.Set as Set
import Palladio
import Security

import Misc



{- ... unter Verwendung folgender "Hilfsbegriffe" ... -}
accessibleParameters :: (AbstractDesignModel m) => (Attacker m) -> Set (Parameter m)
accessibleParameters attacker = fromList $
  -- Ausgabe-Parameter, auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat
  [ parameter | interface <- (interfacesAllowedToBeUsedBy attacker ⋅),
                interface ∈  systemProvides,
                method    <- (methods interface⋅),
                parameter <- (outputParameters method⋅)
  ] ++

  -- Eingabe-Parameter,auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat, weil er vom System Aufgerufen wird.
  [ parameter | interface <- (interfacesAllowedToBeUsedBy attacker ⋅),
                interface ∈  systemRequires,
                method    <- (methods interface ⋅),
                parameter <- (inputParameters method ⋅)
  ] ++

  -- Parameter, auf die der Angreifer Zugriff hat, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ parameter | container <- (containersFullyAccessibleBy attacker ⋅),
                interface <- ((providedInterfacesOn container) ∪ (requiredInterfacesOn container) ⋅),
                method    <- (methods interface⋅),
                parameter <- ((inputParameters method) ∪ (outputParameters method) ⋅)
  ] ++

  -- Parameter, auf die der Angreifer Zugriff hat, weil er eine entsprechende LinkResource angreifen konnte.
  [ parameter | link                  <- (linksPayloadFullyAccessibleBy attacker⋅),
                let (containerLeft,
                     containerRight)   = linkBetween link,
                left                   <- (assembliesOn containerLeft ⋅),
                interface              <- (requires (componentOf left) ⋅),
                let (ByAssembly right) = systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                method                 <- (methods interface ⋅),
                parameter              <- ((inputParameters method) ∪ (outputParameters method) ⋅)
  ]


observableServices :: (AbstractDesignModel m) => (Attacker m) -> Set (Service m)
observableServices attacker = fromList $
  -- Services, deren Aufrufe der Angreifer als regulärer "Benutzer" des Systems beobachten kann
  [ service | interface <- (interfacesAllowedToBeUsedBy attacker ⋅),
              interface ∈  systemProvides,
              service   <- (methods interface ⋅)
  ] ++
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ service | container <- (containersFullyAccessibleBy attacker ⋅),
              interface <- ((providedInterfacesOn container) ∪ (requiredInterfacesOn container) ⋅),
              service   <- (methods interface⋅)
  ] ++
  -- Services, deren Aufrufe der Angreifer beobachten kann, weil er eine entsprechende LinkResource angreifen konnte.
  [ service   | link                  <- (linksMetaDataFullyAccessibleBy attacker⋅),
                let (containerLeft,
                     containerRight)   = linkBetween link,
                left                   <- (assembliesOn containerLeft ⋅),
                interface              <- (requires (componentOf left) ⋅),
                let (ByAssembly right) = systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                service                <- (methods interface ⋅)
  ]





{- Denn damit kann man direkt ein Analyserusultat bestimmen: -}
instance (AbstractDesignModel m) => AnalysisResult m where
  dataAccessibleTo attacker = fromList $
    [ classificationOf parameter  | parameter <- (accessibleParameters attacker ⋅)] ++
    [ classificationOfCall method | method    <- (observableServices attacker ⋅)]
