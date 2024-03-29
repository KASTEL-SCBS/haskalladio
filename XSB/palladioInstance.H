:- export  % Predicates
           systemProvides/1,      % systemProvides(Interface)
           systemRequires/1,      % systemRequires(Interface)
           componentOf/2,         % componentOf(AssemblyContext,Component)
           systemAssembledTo/3,   % systemAssembledTo(AssemblyContext                 -- the assembly context
                                  %                   Interface                       -- an interface required by the assemblys component
                                  %                   AssemblyRequirementSatisfaction -- Another assembly resolving that requirement,
                                  %                                                   -- Unless requirement becomes a system requirement.
           systemProvidesAssembledTo/2, % systemProvidesAssembledTo(Interface,       -- an interface provided by the system
                                         %                            AssemblyContext) -- the assembly providing that interface
           runsOn/2,              % runsOn(AssemblyContext,ResourceContainer)
           connects/3,         % connects(LinkingResource,ResourceContainer, ResourceContainer)

           % Enumerators
           resourcecontainers/1,  % resourcecontainers(ResourceContainer)
           linkingresources/1,    % linkingresources(LinkingResource )
           system/1,              % system(AsssemblyContext)

           % Used Functors
           % AssemblyRequirementSatisfaction = asSystemRequirement/0 | byAssembly/1
           asSystemRequirement/0, %
           byAssembly/1.          %  byAssembly(AssemblyContext)
