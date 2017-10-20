:- export  % Predicates
           withVar/2,
           specificationParameterEquivalences/1,
           specificationParameterEquivalencesWithAssignment/2,
           specificationParameterEquivalencesWithAssignments/1,
           equal/2,
           specificationParametersAt/3,             % specificationParametersAt(AssemblyContext, Interface, SpecificationParameter).
           assigned/2,                              % assigned((AssemblyContext, Interface, SpecificationParameter), Key)
           uiInterfaceOn/2,                         % uiInterfaceOn(ResourceContainer,Interface)
           mayknow/2,             % mayknow(Attacker,DataSet)
           includes/4,                      % includes(Parameter, DataSet, AssembluContext, World)
           includesCall/4,                  % includesCall(Service, DataSet, AssembluContext, World)
           mylookup/3,
           unifyAll/2,
           lookupDataSets/3,
           sworld/1,
           lookupDataSetParameterNames/6,
           dataSetMapEntryFor/2,
           assignedBy/3,
           assignAll/2,
           specificationParameterConstraints/2,
           merge/3,
           mergeAll/2,
           toList/2,

           tamperingAbilities/3,                    % tamperingAbilities(Attacker,Location,TamperingAbility)
           locationsAccessibleBy/2,                 % locationsAccessibleBy(Attacker,Location)
           furtherConnections/2,                    % furtherConnections(ResourceContainer,FurtherConnections)
           sharingOf/2,                             % sharingOf(ResourceContainer,Sharing)
           isLocation/1,                            % isLocation(Location)
           location/3,                              % location(ResourceContainer,Location,TamperingAbility)
           linkLocation/3,                          % linkLocation(LinkingResource,Location,TamperingAbility)
           contextOfConnector/3,
           % Enumerators
           dataset/1,    % dataset(DataSet)
           attacker/1.   % attacker(Attacker)
