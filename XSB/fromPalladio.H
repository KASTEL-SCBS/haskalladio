:- export  % Predicates
        % Currently unused: adversaries/1
        % Currently unused: allocation/1
        adversary/1,                                % adversary(Attacker)
        allocationContext/1,                        % allocationContext(AllocationContext)
        assemblyConnector/1,                        % assemblyConnector(Connector)
        assemblyContext/1,                          % assemblyContext(Context)
        basicComponent/1,                           % basicComponent(Component)
        % Currently unused: communicationLinkResourceSpecifications/2
        % Currently unused: components/2
        connectedResourceContainers/2               % connectedResourceContainers(Link,[RCLeft,RCRight])
        connectionType/2,                           % connectionType(Container,Mode)
        dataSet/1,                                  % dataSet(Dataset)
        dataSetTargets/2,                           % dataSetTargets(ParametersAndDataSetsPair, Datasets),
        % Currently unused: dataSets/2
        dataType/2,                                 % dataType(Parameter,Type)
        encapsulatedComponent/2,                    % encapsulatedComponent(AssemblyContext,Component)
        innerProvidedRole/2,                        % innerProvidedRole(Connector,InnerProvidedRole)
        % Currently unused: interfaces/2
        linkingResource/1,                          % linkingResource(LinkingResource)
        % Currently unused: linkingResources/2
        location/1,                                 % location(Location)
        locations/2,                                % locations(LocationsAndTamperProtectionsPair,Locations),
        % Currently unused: locationsAndTamperProtectionsPair/1
        locationsAndTamperProtectionsPairs/2        % locationsAndTamperProtectionsPairs(LinkingResource,LocationsAndTamperProtectionsPairs),
        % Currently unused: mayKnowDataSets/2
        % Currently unused: confidentialitySpecification/1
        % Currently unused: operationInterface/1
        operationProvidedRole/1,                    % operationProvidedRole(InnerProvidedRole)
        % Currently unused: operationRequiredRole/1 TODO: why is this unused, while operationProvidedRole/1 *is* used?
        % Currently unused: operationSignature/1
        outerProvidedRole/2,                        % outerProvidedRole(Connector,OuterProvidedRole)
        % Currently unused: parameter/1
        parameterSources/2,                         % parameterSources(Pair,ParameterNames)
        parameters/2,                               % parameters(Service, Parameters)
        % Currently unused: parametersAndDataSetsPair/1
        parametersAndDataSetsPairs/2,               % parametersAndDataSetsPairs(Service, Pairs)
        providedDelegationConnector/1,              % providedDelegationConnector(Connector)
        providedInterface/2,                        % providedInterface(InnerProvidedRole,Interface)
        providedRoles/2,                            % providedRoles(Component,Roles)
        providingAssemblyContext/2,                 % providingAssemblyContext(Connector, ProvidingContext)
        % Currently unused: repository/1
        requiredInterface/2,                        % requiredInterface(Role,Interface)
        requiredRoles/2,                            % requiredRoles(Component,Roles)
        requiringAssemblyContext/2,                 % requiringAssemblyContext(Connector, RequiringContext)
        resourceContainer/1,                        % resourceContainer(Container)
        resourceContainer/2,                        % resourceContainer(AllocationContext,Container)
%       resourceContainer/2,                        % resourceContainer(ResourceEnvironment, Containers) TODO: this 2nd meaning  of resourceContainer/2 is currently exported from Palladio, but not used in inference rules
        unencryptedDataSets/2,                      % unencryptedDataSets(Link, Datasets)
        returnType/2,                               % returnType(Service,Type)
        sharing/2,                                  % sharing(Container,Mode) % TODO: fix the situation where a predicate sharing/2 is also defined elsewhere
        signatures/2,                               % signatures(Interface,Services)
        parametersAndDataSetsPairs/2,               % parametersAndDataSetsPairs(Service, Pairs)
        % Currently unused: repository/1. In fact, we shouldn't  export this, since there already is a predicate system/1 elsewhere
        % Currently unused: tamperProtection/1
        tamperProtections/2.                        % tamperProtections(Pair, TamperProtections)
