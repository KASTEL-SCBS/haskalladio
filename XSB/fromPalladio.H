:- export  % Predicates
        assignments/2,                              % assignements(informationFlowParameterAssignment, [DataSetMapParameter2KeyAssignment])
        assignedKey/2,                              % assignedKey(dataSetMapParameter2KeyAssignment,["A"]).
        adversary/1,                                % adversary(Attacker)
        allocationContext/1,                        % allocationContext(AllocationContext)
        assemblyConnector/1,                        % assemblyConnector(Connector)
        assemblyContext/1,                          % assemblyContext(AssemblyContext)
        assemblyContext/2,                          % assemblyContext(ProvidedDelegationConnector,AssemblyContext)
        assemblyContext_AllocationContext/2,        % assemblyContext_AllocationContext(AllocationContext,AssemblyContext)
        basicComponent/1,                           % basicComponent(Component)
        connectionType/2,                           % connectionType(Container,[Mode])
        connectedResourceContainers/2,              % connectedResourceContainers(Link,[RCLeft,RCRight])
        connectorSpecificParametersAndDataPairs/2,  % connectorSpecificParametersAndDataPairs(AssemblyConnector,ParametersAndDataPair)
        dataSet/1,                                  % dataSet(Dataset)
        dataSetMapEntry/1,                          % dataSetMapEntry(DatasetMapEntry)
        dataTargets/2,                              % dataTargets(ParametersAndDataSetsPair, Datasets),
        dataSetMapParameter2KeyAssignment/1,        % dataSetMapParameter2KeyAssignment(DataSetMapParameter2KeyAssignment).
	description/3,
        encapsulatedComponent/2,                    % encapsulatedComponent(AssemblyContext,[Component])
        equations/2,                                % equations(AssemblyContext, [SpecificationParameterEquation]),
        innerProvidedRole/2,                        % innerProvidedRole(Connector,[InnerProvidedRole])
        linkingResource/1,                          % linkingResource(LinkingResource)
        leftInterfaces/2,                           % leftInterfaces(SpecificationParameterEquation, [Interface])
        location/1,                                 % location(Location)
        locations/2,                                % locations(LocationsAndTamperProtectionsPair,Locations),
        locationsAndTamperProtectionsPairs/2,       % locationsAndTamperProtectionsPairs(LinkingResource,LocationsAndTamperProtectionsPairs),
        mayKnowData/2,                              % mayKnowData(Attacker, Datasets)
	nameFor/2,                                  % nameFor(DatasetMapEntry, ["A"]).
        operationProvidedRole/1,                    % operationProvidedRole(InnerProvidedRole)
        operationRequiredRole/1,                    % operationRequiredRole(InnerRequiredRole)
	originalParametersAndDataPair/2,            % originalParametersAndDataPair(ConnectorSpecificPair, OriginalPair)
        outerProvidedRole/2,                        % outerProvidedRole(Connector,[OuterProvidedRole])
        parameter/2,                                % parameter(ParameterizedDataSetMapEntry, SpecificationParameter)
        parameters/2,                               % parameters(Service, Parameters)
        parameterSources/2,                         % parameterSources(Pair,ParameterNames)
        parametersAndDataPairs/2,                   % parametersAndDataPairs(Service, Pairs)
        parameterizedDataSetMapEntry/1,             % parameterizedDataSetMapEntry(ParameterizedDataSetMapEntry)
        providedDelegationConnector/1,              % providedDelegationConnector(Connector)
        providedInterface/2,                        % providedInterface(InnerProvidedRole,[Interface])
        providedRole/2,                             % providedRole(Connector, [ProvidedRole]),
        providedRoles/2,                            % providedRoles(Component,Roles)
        providingAssemblyContext/2,                 % providingAssemblyContext(Connector, [ProvidingContext])
        leftSpecificationParameter/2,               % leftSpecificationParameter(SpecificationParameterEquation, [SpecificationParameter]).
        requiredDelegationConnector/1,              % providedDelegationConnector(Connector)
        requiredInterface/2,                        % requiredInterface(Role,Interface)
        requiredRole/2,                             % requiredRole(Connector, [RequiredRole])
        requiredRoles/2,                            % requiredRoles(Component,Roles)
        requiringAssemblyContext/2,                 % requiringAssemblyContext(Connector, [RequiringContext])
        rightSpecificationParameter/2,              % rightSpecificationParameter(SpecificationParameterEquation, [SpecificationParameter]).
        resourceContainer/1,                        % resourceContainer(Container)
        resourceContainer/2,                        % resourceContainer(AllocationContext,Container)
        returnType/2,	                            % returnType(Service,Type)
        rightInterfaces/2,                          % rightInterfaces(SpecificationParameterEquation, [Interface])
	serviceParameters/2,                        % serviceParameters(Interface,ServiceParameters)
        specificationParameterEquation/1,           % specificationParameterEquation(SpecificationParameterEquation)
        specificationParameter/1,                   % specificationParameter(SpecificationParameter)
        specificationParameters/2,                  % specificationParameters(Interface,[SpecificationParameter]).
        specificationParametersToReplace/2,         % specificationParametersToReplace(DataSetMapParameter2KeyAssignment, [SpecificationParameter])
        signatures/2,                               % signatures(Interface,Services)
        sharing/2,                                  % sharing(Container,[Mode]) % TODO: fix the situation where a predicate sharing/2 is also defined elsewhere
        tamperProtections/2,                        % 
        unencryptedDataSets/2,                      % unencryptedDataSets(Link, Datasets)
        outerRequiredRole/2,                        % outerRequiredRole(Connector,ProvidedRole)

        sizeOf/1,
	call/0,
	return/0,
        fake_usage/0.



% Currently unused:
:- local
        addedServiceParameter/1,                    % addedServiceParameter(ServiceParamater)
        addedServiceParameters/2,
	collectionDataType/1,                       %
	compositeDataType/1,                        %
%         dataType/2,                                 % dataType(Parameter,Type)
%         innerRequiredRole/2,                        % innerProvidedRole(Connector,InnerRequiredRole)
        parameter/1,                                % parameter(ParameterName)
%       resourceContainer/2,                        % resourceContainer(ResourceEnvironment, Containers) TODO: this 2nd meaning  of resourceContainer/2 is currently exported from Palladio, but not used in inference rules
        sizeOfParameter/1, 	                    % sizeOfParameter(SizeOfParameterName)
        sizeOf/2,                                   % sizeOf(SizeOfParameterName, ParameterName)


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
        interfaces/2,
        linkingResources/2,
        locationsAndTamperProtectionsPair/1,
        confidentialitySpecification/1,
	map/2,                                      % map(ParameterizedDataSetMapEntry, DataSetMap)
        operationInterface/1,
        operationSignature/1,
        parameter/1,
        parametersAndDataPair/1,
	parentInterfaces/2,
        repository/1,
        resourceEnvironment/1,
        system/1,  % In fact, we must not export this, since there already is a predicate system/1 elsewhere
        specificationParameterAssignments/2,        % specificationParameterAssignments(ConfidentialitySpecification, [DataSetMapParameter2KeyAssignment]).
        specificationParameterEquations/2,          % specificationParameterAssignments(ConfidentialitySpecification, [SpecificationParameterEquation]).
        tamperProtection/1.
