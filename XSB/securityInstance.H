:- export  % Predicates
           withVar/2,
           equal/2,
           specificationParametersAt/3,             % specificationParametersAt(AssemblyContext, Interface, SpecificationParameter).
           assigned/2,                              % assigned((AssemblyContext, Interface, SpecificationParameter), Key)
           uiInterfaceOn/2,                         % uiInterfaceOn(ResourceContainer,Interface)
           mayknow/2,             % mayknow(Attacker,DataSet)
           includes/4,                      % includes(Parameter, DataSet, AssembluContext, World)
           includesCall/4,                  % includesCall(Service, DataSet, AssembluContext, World)
           mylookup/3,
           sworld/1,
           sconstraints/2,
           assignedBy/4,
           toList/2,
           connectorFor/2,
	   worldFor/3,
	   required/1,
	   provided/1,

           % For Debugging:
           lookupDataSetParameterNames/5,

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
