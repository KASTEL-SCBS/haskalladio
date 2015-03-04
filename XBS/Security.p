:- import  exposesPhsicallyAccessiblePayloadTo/2,
           exposesPhsicallyAccessibleMetaDataTo/2 from LinkingModel.
:- import  dataAccessibleTo/2 from AbstractAnalysis.
:- import  interfacesAllowedToBeUsedBy/2,
           dataAllowedToBeAccessedBy/2,
           classificationOf/2,
           classificationOfCall/2,
           tamperingAbilities/2,
           locationsAccessibleBy/2,
           containerSecuredByMethod/2,
           furtherConnections/2,
           sharing/2,
           location/2,
           linkLocation/2,
           dataset/1,
           attacker/1
           from SecurityInstance.





:- export  % Predicates
           isInSecureWithRespectTo/1,               % isInSecureWithRespectTo(Attacker)

           containersFullyAccessibleBy/2            % containersFullyAccessibleBy(Attacker,ResourceContainer)
           linksMetaDataFullyAccessibleBy/2         % linksMetaDataFullyAccessibleBy(Attacker,LinkingResource)
           linksPayloadFullyAccessibleBy/2,         % linksPayloadFullyAccessibleBy(Attacker,LinkingResource)

           % Used Functors
           % Sharing = OpenShared | ControlledExclusive
           openShared/0, controlledExclusive/0,
           % FurtherConnections = Possible | Existing | Complete
           possible/0, existing/0, complete/0,


isInSecureWithRespectTo(Attacker) :-
        dataAccessibleTo(Attacker,Accessed),
        not (dataAllowedToBeAccessedBy(Attacker,Accessed).


containersFullyAccessibleBy(Attacker,Container) :-
        containersPhysicalAccessibleBy(Attacker,Container),
        containerSecuredByMethod(Container,Method),
        tamperingAbilities(Attacker,Method).
containersFullyAccessibleBy(_,Container) :-
        sharing(Container,openShared),
        furtherConnections(Container,existing).
containersFullyAccessibleBy(Attacker,Container) :-
        containersPhysicalAccessible(Attacker,Container),
        sharing(Container,openShared),
        furtherConnections(Container,possible).


linksPayloadFullyAccessibleBy(Attacker,Link) :-
    linksPhysicalAccessibleBy(Attacker,Link),
    exposesPhsicallyAccessiblePayloadTo(Link,Attacker).

linksMetaDataFullyAccessibleBy(Attacker,Link) :-
    linksPhysicalAccessibleBy(Attacker,Link),
    exposesPhsicallyAccessibleMetaDataTo(Link,Attacker).

