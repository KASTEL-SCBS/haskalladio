:- export  % Predicates
        adversary/1,                                % adversary(Attacker)
        assemblyContext/2,                          % assemblyContext(ProvidedDelegationConnector,AssemblyContext)
        assemblyContext_AllocationContext/2,        % assemblyContext_AllocationContext(AllocationContext,AssemblyContext)
        connectionType/2,                           % connectionType(Container,[Mode])
        dataSet/1,                                  % dataSet(Dataset)
        dataSetMapEntry/1,                          % dataSetMapEntry(DatasetMapEntry)
        linkingResource/1,                          % linkingResource(LinkingResource)
        location/1,                                 % location(Location)
        locations/2,                                % locations(LocationsAndTamperProtectionsPair,Locations),
        locationsAndTamperProtectionsPairs/2,       % locationsAndTamperProtectionsPairs(LinkingResource,LocationsAndTamperProtectionsPairs),
        mayKnowData/2,                              % mayKnowData(Attacker, Datasets)
        outerProvidedRole/2,                        % outerProvidedRole(Connector,[OuterProvidedRole])
        parameterSources/2,                         % parameterSources(Pair,ParameterNames)
        parametersAndDataPairs/2,                   % parametersAndDataPairs(Service, Pairs)
        providedInterface/2,                        % providedInterface(InnerProvidedRole,[Interface])
        requiredInterface/2,                        % requiredInterface(Role,Interface)
        resourceContainer/1,                        % resourceContainer(Container)
        resourceContainer/2,                        % resourceContainer(AllocationContext,Container)
        sharing/2,                                  % sharing(Container,[Mode]) % TODO: fix the situation where a predicate sharing/2 is also defined elsewhere
        tamperProtections/2,                        % 
        outerRequiredRole/2,                        % outerRequiredRole(Connector,ProvidedRole)

        fake_usage/0.



% Currently unused:
:- local

        allocationContext/1,                        % allocationContext(AllocationContext)
        assemblyConnector/1,                        % assemblyConnector(Connector)
        assemblyContext/1,                          % assemblyContext(AssemblyContext)
        basicComponent/1,                           % basicComponent(Component)
        connectedResourceContainers/2,              % connectedResourceContainers(Link,[RCLeft,RCRight])
        dataTargets/2,                              % dataSetTargets(ParametersAndDataSetsPair, Datasets),
%         dataType/2,                                 % dataType(Parameter,Type)
        encapsulatedComponent/2,                    % encapsulatedComponent(AssemblyContext,[Component])
        innerProvidedRole/2,                        % innerProvidedRole(Connector,[InnerProvidedRole])
%         innerRequiredRole/2,                        % innerProvidedRole(Connector,InnerRequiredRole)
        operationProvidedRole/1,                    % operationProvidedRole(InnerProvidedRole)
        operationRequiredRole/1,                    % operationRequiredRole(InnerRequiredRole)
        parameters/2,                               % parameters(Service, Parameters)
        parameter/1,                                % parameter(ParameterName)
        providedDelegationConnector/1,              % providedDelegationConnector(Connector)
        providedRoles/2,                            % providedRoles(Component,Roles)
        providedRole/2,                             % providedRole(Connector, [ProvidedRole]),
        providingAssemblyContext/2,                 % providingAssemblyContext(Connector, [ProvidingContext])
%         requiredDelegationConnector/1,              % providedDelegationConnector(Connector)
        requiredRoles/2,                            % requiredRoles(Component,Roles)
        requiredRole/2,                             % requiredRole(Connector, [RequiredRole])
        requiringAssemblyContext/2,                 % requiringAssemblyContext(Connector, [RequiringContext])
%       resourceContainer/2,                        % resourceContainer(ResourceEnvironment, Containers) TODO: this 2nd meaning  of resourceContainer/2 is currently exported from Palladio, but not used in inference rules
%         unencryptedDataSets/2,                      % unencryptedDataSets(Link, Datasets)
        returnType/2,                               % returnType(Service,Type)
        sizeOfParameter/1, 	                    % sizeOfParameter(SizeOfParameterName)
        sizeOf/2,                                   % sizeOf(SizeOfParameterName, ParameterName)
        signatures/2,                               % signatures(Interface,Services)

	% Constants
	sizeOf/1,


	dataIdentifier/2,
	dataSetMaps/2,
        adversaries/1,
        adversaries/2,
        allocation/1,
        allocationContexts/2,
        assemblyContexts/2,
        communicationLinkResourceSpecifications/2,
        components/2,
        connectors/2,
        connectorSpecificParametersAndDataPairs/2,  % connectorSpecificParametersAndDataPairs(AssemblyConnector,ParametersAndDataPair)
        interfaces/2,
        linkingResources/2,
        locationsAndTamperProtectionsPair/1,
        confidentialitySpecification/1,
	map/2,
        operationInterface/1,
        operationSignature/1,
        parameter/1,
        parametersAndDataPair/1,
	parentInterfaces/2,
        repository/1,
        resourceEnvironment/1,
        system/1,  % In fact, we must not export this, since there already is a predicate system/1 elsewhere
        tamperProtection/1.
