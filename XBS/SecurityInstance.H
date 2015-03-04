:- export  % Predicates
           interfacesAllowedToBeUsedBy/2            % interfacesAllowedToBeUsedBy(Attacker,Interface)
           dataAllowedToBeAccessedBy/2              % dataAllowedToBeAccessedBy(Attacker,DataSet)
           classificationOf/2,                      % classificationOf(Parameter,DataSet)
           classificationOfCall/2                   % classificationOfCall(Service,DataSet m)

           tamperingAbilities/2                     % tamperingAbilities(Attacker,TamperingAbility)
           locationsAccessibleBy/2                  % locationsAccessibleBy(Attacker,Location)
           containerSecuredByMethod/2               % containerSecuredByMethod(ResourceContainer,TamperingAbility)
           furtherConnections/2                     % furtherConnections(ResourceContainer,FurtherConnections)
           sharing/2                                % sharing(ResourceContainer,Sharing)
           location/2                               % location(ResourceContainer,Location)
           linkLocation/2                           % linkLocation(LinkingResource,Location)

           % Enumerators
           dataset/1,    % dataset(DataSet)
           attacker/1,   % attacker(Attacker)

