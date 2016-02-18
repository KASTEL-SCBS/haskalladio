:- export  % Predicates
           uiInterfaceOn/2,                         % uiInterfaceOn(ResourceContainer,Interface)
           mayknow/2,             % mayknow(Attacker,DataSet)
           includes/2,                      % includes(Parameter,DataSet)
           includesCall/2,                  % includesCall(Service,DataSet m)

           tamperingAbilities/3,                    % tamperingAbilities(Attacker,Location,TamperingAbility)
           locationsAccessibleBy/2,                 % locationsAccessibleBy(Attacker,Location)
           furtherConnections/2,                    % furtherConnections(ResourceContainer,FurtherConnections)
           sharingOf/2,                             % sharingOf(ResourceContainer,Sharing)
           isLocation/1,                            % isLocation(Location)
           location/3,                              % location(ResourceContainer,Location,TamperingAbility)
           linkLocation/3,                          % linkLocation(LinkingResource,Location,TamperingAbility)

           % Enumerators
           dataset/1,    % dataset(DataSet)
           attacker/1.   % attacker(Attacker)
