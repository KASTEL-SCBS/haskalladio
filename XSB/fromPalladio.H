:- export  % Predicates
        adversary/1,                                % adversary(Attacker)
        allocationContext/1,                        % allocationContext(AllocationContext)
        assemblyConnector/1,                        % assemblyConnector(Connector)
        assemblyContext/1,                          % assemblyContext(AssemblyContext)
        assemblyContext/2,                          % assemblyContext(Connector,AssemblyContext)
        assemblyContext_AllocationContext/2,        % assemblyContext_AllocationContext(AllocationContext,AssemblyContext)
        basicComponent/1,                           % basicComponent(Component)
        connectedResourceContainers/2,              % connectedResourceContainers(Link,[RCLeft,RCRight])
        connectionType/2,                           % connectionType(Container,Mode)
        dataSet/1,                                  % dataSet(Dataset)
        dataSetTargets/2,                           % dataSetTargets(ParametersAndDataSetsPair, Datasets),
        dataType/2,                                 % dataType(Parameter,Type)
        encapsulatedComponent/2,                    % encapsulatedComponent(AssemblyContext,Component)
        innerProvidedRole/2,                        % innerProvidedRole(Connector,InnerProvidedRole)
        innerRequiredRole/2,                        % innerProvidedRole(Connector,InnerRequiredRole)
        linkingResource/1,                          % linkingResource(LinkingResource)
        location/1,                                 % location(Location)
        locations/2,                                % locations(LocationsAndTamperProtectionsPair,Locations),
        locationsAndTamperProtectionsPairs/2,       % locationsAndTamperProtectionsPairs(LinkingResource,LocationsAndTamperProtectionsPairs),
        mayKnowDataSets/2,                          % mayKnowDataSets(Attacker, Datasets)
        operationProvidedRole/1,                    % operationProvidedRole(InnerProvidedRole)
        operationRequiredRole/1,                    % operationRequiredRole(InnerRequiredRole)
        outerProvidedRole/2,                        % outerProvidedRole(Connector,OuterProvidedRole)
        outerRequiredRole/2,                        % outerRequiredRole(Connector,ProvidedRole)
        parameterSources/2,                         % parameterSources(Pair,ParameterNames)
        parameters/2,                               % parameters(Service, Parameters)
        parametersAndDataSetsPairs/2,               % parametersAndDataSetsPairs(Service, Pairs)
        providedDelegationConnector/1,              % providedDelegationConnector(Connector)
        providedInterface/2,                        % providedInterface(InnerProvidedRole,Interface)
        providedRoles/2,                            % providedRoles(Component,Roles)
        providedRole/2,                             % providedRole(Connector, ProvidedRole),
        providingAssemblyContext/2,                 % providingAssemblyContext(Connector, ProvidingContext)
        requiredDelegationConnector/1,              % providedDelegationConnector(Connector)
        requiredInterface/2,                        % requiredInterface(Role,Interface)
        requiredRoles/2,                            % requiredRoles(Component,Roles)
        requiredRole/2,                             % requiredRole(Connector, RequiredRole)
        requiringAssemblyContext/2,                 % requiringAssemblyContext(Connector, RequiringContext)
        resourceContainer/1,                        % resourceContainer(Container)
        resourceContainer/2,                        % resourceContainer(AllocationContext,Container)
%       resourceContainer/2,                        % resourceContainer(ResourceEnvironment, Containers) TODO: this 2nd meaning  of resourceContainer/2 is currently exported from Palladio, but not used in inference rules
        unencryptedDataSets/2,                      % unencryptedDataSets(Link, Datasets)
        returnType/2,                               % returnType(Service,Type)
        sharing/2,                                  % sharing(Container,Mode) % TODO: fix the situation where a predicate sharing/2 is also defined elsewhere
        signatures/2,                               % signatures(Interface,Services)
        parametersAndDataSetsPairs/2,               % parametersAndDataSetsPairs(Service, Pairs)
        tamperProtections/2,                        % tamperProtections(Pair, TamperProtections)

        fake_usage/0.



% Currently unused:
:- local
        adversaries/1,
        adversaries/2,
        allocation/1,
        allocationContexts/2,
        assemblyContexts/2,
        communicationLinkResourceSpecifications/2,
        components/2,
        connectors/2,
        dataSets/2,
        interfaces/2,
        linkingResources/2,
        locationsAndTamperProtectionsPair/1,
        confidentialitySpecification/1,
        operationInterface/1,
        operationSignature/1,
        parameter/1,
        parametersAndDataSetsPair/1,
        repository/1,
        resourceEnvironment/1,
        system/1,  % In fact, we shouldn't  export this, since there already is a predicate system/1 elsewhere
        tamperProtection/1.
