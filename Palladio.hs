{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-}

module Palladio where

import Data.Set as Set

type Repository m = Set (Component m)

data AssemblyRequirementSatisfaction a = AsSystemRequirement | ByAssembly a


implies a b = (not a) || b

class (Ord (Parameter m),
       Ord (Method m),
       Ord (LinkingResource m),
       Ord (Interface m),
       Ord (ResourceContainer m),
       Ord (AssemblyContext m),
       Ord (Component m)  ) => PalladioComponentModel m where
  data Component m
  data Interface m
  data Parameter m
  data Method m
  data DataType m
  data AssemblyContext m
  data ResourceContainer m
  data LinkingResource m
  

  
  methods :: Interface m -> Set (Method m)
  inputParameters :: Method m -> Set (Parameter m)
  outputParameters :: Method m -> Set (Parameter m)
  typeOf :: Parameter m -> DataType m
  
  
  components :: Repository m

  provides :: Component m -> Set (Interface m)
  requires :: Component m -> Set (Interface m)
  
  isProvided :: Component m -> Bool
  isComplete :: Component m -> Bool
  
  isBasic :: Component m -> Bool
  
  subComponents :: Component m -> Set (Component m)
  
  assembledTo ::    Component m  -- the composite component
                   -> Component m  -- a sub component
                   -> Interface m  -- a required Interface by the sub component
                   -> Component m  -- another sub component providing that interface

  delegatesProvides ::  Component m  -- the composite component
                      -> Interface m  -- an Interface provided by the composite component
                      -> Component m  -- the sub component actually providing it
  
  delegatesRequires  ::  Component m  -- the composite component
                      -> Interface m  -- an Interface required by the composite component
                      -> Set (Component m)  -- the sub components actually requiring it
  
  
  

  
  system :: Set (AssemblyContext m)
  
  systemProvides :: Set (Interface m)
  systemRequires :: Set (Interface m)

  componentOf :: AssemblyContext m -> Component m
  
  
  systemAssembledTo ::    AssemblyContext m  -- the assembly context
                       -> Interface m       -- an interface required by the assemblys component
                       -> AssemblyRequirementSatisfaction (AssemblyContext m) -- Another assembly resolving that requirement,
                                                                              -- Unless requirement becomes a system requirement.
  
  systemProvidesAsssembledTo ::    Interface m       -- an interface provided by the system
                                -> AssemblyContext m -- the assembly providing that interface

  resourcecontainers :: Set (ResourceContainer m)
  linkingresources :: Set (LinkingResource m)
  
  runsOn :: AssemblyContext m -> ResourceContainer m
  
  linkBetween :: LinkingResource m -> (ResourceContainer m, ResourceContainer m)

isComposite :: PalladioComponentModel m => Component m -> Bool
isComposite = not . isBasic


assembliesOn :: PalladioComponentModel m => ResourceContainer m -> Set (AssemblyContext m)
assembliesOn container = fromList $
  [ assembly | assembly <- elems system,
               runsOn assembly ==  container
  ]

providedInterfacesOn :: PalladioComponentModel m => ResourceContainer m -> Set (Interface m)
providedInterfacesOn container = fromList $
  [ interface   | assembly <- elems system,
                  let container = runsOn assembly
                      component = componentOf assembly,
                  interface <- elems $ provides component
  ]
requiredInterfacesOn :: PalladioComponentModel m => ResourceContainer m -> Set (Interface m)
requiredInterfacesOn container = fromList $
  [ interface   | assembly <- elems system,
                  let container = runsOn assembly
                      component = componentOf assembly,
                  interface <- elems $ requires component
  ]



wellFormed ::  PalladioComponentModel m => Component m  -> Bool
wellFormed c = providesWellFormed c &&
               completeWellFormed c &&
               subComponentsWellFormed c
  where
    providesWellFormed c
     | isProvided c =      Set.null (requires c) &&
                      not (Set.null (provides c))
     | otherwise    = True

    completeWellFormed c = True

    subComponentsWellFormed c =
       (isBasic c) == (Set.null (subComponents c)) && (
       implies (isComplete c) $
         and [ required `Set.member` (provides providing) | sub <- elems $ subComponents c,
                                                        required  <- elems $ requires sub,
                                                        providing <- [assembledTo c sub required]
         ] &&
         and [    (delegatesProvides c interface) `Set.member` (subComponents c)
               && (interface `Set.member` provides (delegatesProvides c interface))
         | interface <- elems $ provides c ]
      ) && True -- TODO: check consistency of delegatesRequired


-- TODO: wellFormedSystem, insbesondere:: konsistenz von:   
-- systemRequires :: Set (Interface m)
-- mit
-- systemAssembledTo
