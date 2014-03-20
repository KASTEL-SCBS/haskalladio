-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE GADTs #-}  -- für Typklassenkontexte in PalladioComponentModel
-- {-# LANGUAGE DatatypeContexts #-}


import Data.Set as Set

data AssemblyRequirementSatisfaction a = AsSystemRequirement | ByAssembly a

data PalladioComponentModel m component interface parameter method datatype  assemblycontext resourcecontainer linkingresource 
 = Model {
  methods  :: interface m -> Set (method m),
  inputparameters  :: method m -> Set (parameter m),
  outputparameters :: method m -> Set (parameter m),
  typeOf :: parameter m -> datatype m,
     
  components :: (Set (component m)),

  provides :: component m -> Set (interface m),
  requires :: component m -> Set (interface m),
  
  isProvided :: component m -> Bool,
  isComplete :: component m -> Bool,
  
  isBasic :: component m -> Bool,
  
  subComponents :: component m -> Set (component m),
  
  assembledTo ::      component m  -- the composite component
                   -> component m  -- a sub component
                   -> interface m  -- a required interface by the sub component
                   -> component m, -- another sub component providing that interface

  delegatesProvides ::      component m  -- the composite component
                         -> interface m  -- an interface provided by the composite component
                         -> component m, -- the sub component actually providing it
  
  delegatesRequires  ::      component m        -- the composite component
                          -> interface m        -- an interface required by the composite component
                          -> Set (component m), -- the sub components actually requiring it

  system :: Set (assemblycontext m),
  
  systemProvides ::  Set (interface m),
  systemRequires :: Set (interface m),

  componentOf :: assemblycontext m -> component m,
  
  systemAssembledTo ::      assemblycontext m  -- the assembly context
                         -> interface m        -- an interface required by the assemblys component
                         -> AssemblyRequirementSatisfaction (assemblycontext m),-- Another assembly resolving that requirement,
                                                                                -- Unless requirement becomes a system requirement.
  

  resourcecontainers :: Set (resourcecontainer m),
  linkingresources :: Set (linkingresource m),
  
  runsOn :: assemblycontext m -> resourcecontainer m,
  
  linkBetween :: linkingresource m -> (resourcecontainer m, resourcecontainer m)
}


implies a b = (not a) || b




-- PalladioComponentModel m c i p md d a r l
  
isComposite :: PalladioComponentModel m component i p md d a r l -> component m -> Bool
isComposite m  = not . (isBasic m)


assembliesOn :: 
 (Ord (assemblycontext m),
  Eq  (resourcecontainer m)) => 
  PalladioComponentModel m c i p md d assemblycontext resourcecontainer l -> resourcecontainer m -> Set (assemblycontext m)
assembliesOn m container = fromList $
  [ assembly | assembly <- elems $ system m,
               runsOn m assembly == container
  ]

providedInterfacesOn ::
  Ord (interface m) => 
  PalladioComponentModel m c interface p md d a resourcecontainer l -> resourcecontainer m -> Set (interface m)
providedInterfacesOn m container = fromList $
  [ interface   | assembly <- elems $ system m,
                  let container = runsOn m assembly
                      component = componentOf m assembly,
                  interface <- elems $ provides m component
  ]
requiredInterfacesOn ::
  Ord (interface m) =>
  PalladioComponentModel m c interface p md d a resourcecontainer l -> resourcecontainer m -> Set (interface m)
requiredInterfacesOn m container = fromList $
  [ interface   | assembly <- elems $ system m,
                  let container = runsOn m assembly
                      component = componentOf m assembly,
                  interface <- elems $ requires m component
  ]



wellFormed :: (Ord (interface m), Ord (component m)) => PalladioComponentModel m component interface p md d a r l -> component m  -> Bool
wellFormed m c = providesWellFormed c &&
                 completeWellFormed c &&
                 subComponentsWellFormed c
  where
    providesWellFormed c
     | isProvided m c =      Set.null (requires m c) &&
                        not (Set.null (provides m c))
     | otherwise      = True

    completeWellFormed c
     | isComplete m c = not (Set.null (requires m c)) &&
                        not (Set.null (provides m c))
     | otherwise      = True

    subComponentsWellFormed c =
       (isBasic m c) == (Set.null (subComponents m c)) && (
       implies (isComplete m c) $
         and [ required `Set.member` (provides m providing) | sub <- elems $ subComponents m c,
                                                        required  <- elems $ requires m sub,
                                                        providing <- [assembledTo m c sub required]
         ] &&
         and [    (delegatesProvides m c interface) `Set.member` (subComponents m c)
               && (interface `Set.member` provides m (delegatesProvides m c interface))
         | interface <- elems $ provides m c ]
      ) && True -- TODO: check consistency of delegatesRequired


-- TODO: wellFormedSystem, insbesondere:: konsistenz von:   
-- systemRequires :: Set (Interface m)
-- mit
-- systemAssembledTo

