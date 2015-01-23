{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComponentInformationFlow where
import Data.Set as Set
import Palladio
import Security

import Misc

class ComponentRepository m => InformationFlow m where
  possiblyInfluencedBy      :: Parameter m -> Set (Parameter m)
  callsPossiblyInfluencedBy :: Parameter m -> Set (Service m)

  possiblyInfluencedByCallTo      :: Service m -> Set (Parameter m)
  callsPossiblyInfluencedByCallTo :: Service m -> Set (Service m)


isCompatibleWithInformationFlow :: forall m. (BasicDesignModel m, InformationFlow m, PalladioComponentModel m) => Bool
isCompatibleWithInformationFlow = let provided :: (Set (Interface m)) = providedInterfaces
                                      required :: (Set (Interface m)) = requiredInterfaces in

    (∀∈) [ (parameter, influenced) | interface  <- (provided ⋅),
                                     service    <- (services interface ⋅),
                                     parameter  <- (inputParameters service ⋅),
                                     influenced <- (possiblyInfluencedBy parameter ⋅) ]
         (\(parameter, influenced) -> (classificationOf parameter)  ⊆ (classificationOf influenced))
 && (∀∈) [ (parameter, influenced) | interface  <- (provided ⋅),
                                     service    <- (services interface ⋅),
                                     parameter  <- (inputParameters service ⋅),
                                     influenced <- (callsPossiblyInfluencedBy parameter ⋅) ]
         (\(parameter, influenced) -> (classificationOf parameter)  ⊆ (classificationOfCall influenced))
 && (∀∈) [ (service, influenced)   | interface  <- (provided ⋅),
                                     service    <- (services interface ⋅),
                                     influenced <- (possiblyInfluencedByCallTo service ⋅) ]
         (\(service, influenced)   -> (classificationOfCall service) ⊆ (classificationOf influenced))
 && (∀∈) [ (service, influenced)   | interface  <- (provided ⋅),
                                     service    <- (services interface ⋅),
                                     influenced <- (callsPossiblyInfluencedByCallTo service ⋅) ]
         (\(service, influenced) -> (classificationOfCall service)  ⊆ (classificationOfCall influenced))


 && (∀∈) [ (parameter, influenced) | interface  <- (required ⋅),
                                     service    <- (services interface ⋅),
                                     parameter  <- (outputParameters service ⋅),
                                     influenced <- (possiblyInfluencedBy parameter ⋅) ]
         (\(parameter, influenced) -> (classificationOf parameter)  ⊇ (classificationOf influenced))
 && (∀∈) [ (parameter, influenced) | interface  <- (required ⋅),
                                     service    <- (services interface ⋅),
                                     parameter  <- (inputParameters service ⋅),
                                     influenced <- (callsPossiblyInfluencedBy parameter ⋅) ]
         (\(parameter, influenced) -> (classificationOf parameter)  ⊇ (classificationOfCall influenced))
 && (∀∈) [ (service, influenced)   | interface  <- (required ⋅),
                                     service    <- (services interface ⋅),
                                     influenced <- (possiblyInfluencedByCallTo service ⋅) ]
         (\(service, influenced)   -> (classificationOfCall service) ⊇ (classificationOf influenced))
 && (∀∈) [ (service, influenced)   | interface  <- (required ⋅),
                                     service    <- (services interface ⋅),
                                     influenced <- (callsPossiblyInfluencedByCallTo service ⋅) ]
         (\(service, influenced) -> (classificationOfCall service)  ⊆ (classificationOfCall influenced))
