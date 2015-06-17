:- dynamic tamperingAbilities/2.
:- dynamic locationsAccessibleBy/2.
:- dynamic services/2.
:- dynamic inputParameters/2.
:- dynamic outputParameters/2.
:- dynamic typeOf/2.
:- dynamic provides/2.
:- dynamic requires/2.
:- dynamic resourcecontainers/1.
:- dynamic linkingresources/1.
:- dynamic systemModel/1.
:- dynamic systemProvides/2.
:- dynamic systemRequires/2.
:- dynamic systemAssembledTo/3.
:- dynamic systemProvidesAsssembledTo/2.
:- dynamic runsOn/2.
:- dynamic componentOf/2.
:- dynamic linkBetween/3.
:- dynamic interfacesAllowedToBeUsedBy/2.
:- dynamic dataAllowedToBeAccessedBy/2.
:- dynamic classificationOf/2.
:- dynamic classificationOfCall/2.
:- dynamic containerSecuredByMethod/2.
:- dynamic furtherConnections/2.
:- dynamic sharing/2.
:- dynamic location/2.
:- dynamic linkLocation/2.
:- dynamic dataset/1.
:- dynamic attacker/1.
:- dynamic isEncrypted/1.
:- dynamic exposesPhsicallyAccessiblePayloadTo/2.
:- dynamic exposesPhsicallyAccessibleMetaDataTo/2.
:- dynamic accessibleParameters/2.
:- dynamic observableServices/2.
:- dynamic dataAccessibleTo/2.
:- dynamic isInSecureWithRespectTo/1.
:- dynamic containersFullyAccessibleBy/2.
:- dynamic linksMetaDataFullyAccessibleBy/2.
:- dynamic linksPayloadFullyAccessibleBy/2.
:- dynamic linksPhysicalAccessibleBy/2.
:- dynamic containersPhysicalAccessibleBy/2.
:- dynamic providedInterfacesOn/2.
:- dynamic requiredInterfacesOn/2.
:- dynamic interfacesOn/2.
:- dynamic parameters/2.
:- dynamic providedInterfaces/2.
:- dynamic requiredInterfaces/2.

:- dynamic ic/0.
:- dynamic query6/1.

services(currentMeterDataReceiving,storeCurrentConsumption).
services(consumptionDataSending,getCurrentConsumptionConsumptionDataSending).
services(consumptionDataSending,getHistoricConsumption).
services(currentConsumptionDataDisplaying,getCurrentConsumptionCurrentConsumptionDataDisplaying).


inputParameters(storeCurrentConsumption,consumption).

outputParameters(Service,return(Service)) :- Service \= storeCurrentConsumption.

typeOf(consumption,timedConsumption).
typeOf(return(getCurrentConsumptionConsumptionDataSending),timedConsumption).
typeOf(return(getCurrentConsumptionCurrentConsumptionDataDisplaying),timedConsumption).
typeOf(return(getHistoricConsumption),consumptionHistory).

provides(controller,currentMeterDataReceiving).
provides(controller,consumptionDataSending).
provides(tablet,currentConsumptionDataDisplaying).

requires(digitalMeter,currentMeterDataReceiving).
requires(tablet,consumptionDataSending).

resourcecontainers(digitalMeterContainer).
resourcecontainers(controllerContainer).
resourcecontainers(tabletContainer).

linkingresources(digitalMeterContext).
linkingresources(controllerContext).
linkingresources(tabletContext).

systemModel(digitalMeterContext).
systemModel(controllerContext).
systemModel(tabletContext).

systemProvides(currentConsumptionDataDisplaying).

systemRequires(_) :- false.

systemAssembledTo(digitalMeterContext,currentMeterDataReceiving,byAssembly(controllerContext)).
systemAssembledTo(tabletContext,consumptionDataSending,byAssembly(controllerContext)).

systemProvidesAsssembledTo(currentConsumptionDataDisplaying,tabletContext).


runsOn(digitalMeterContext,digitalMeterContainer).
runsOn(controllerContext,controllerContainer).
runsOn(tabletContext,tabletContainer).

componentOf(digitalMeterContext, digitalMeter).
componentOf(controllerContext,controller).
componentOf(tabletContext,tablet).


linkBetween(linkMeterController,digitalMeterContainer, controllerContainer).
linkBetween(linkControllerTablet,controllerContainer, tabletContainer).

dataset(inhabitantData).
dataset(providerData).
dataset(publicData).

attacker(guest).
attacker(burglar).
attacker(handyMan).
attacker(anybody).
attacker(blindDeafGuy).


interfacesAllowedToBeUsedBy(guest,currentConsumptionDataDisplaying).

dataAllowedToBeAccessedBy(guest,publicData).
dataAllowedToBeAccessedBy(guest,inhabitantData).
dataAllowedToBeAccessedBy(handyMan,publicData).
dataAllowedToBeAccessedBy(handyMan,providerData).
dataAllowedToBeAccessedBy(anybody,publicData).
dataAllowedToBeAccessedBy(burglar,publicData).
dataAllowedToBeAccessedBy(blindDeafGuy,publicData).


classificationOf(consumption,inhabitantData).
classificationOf(return(getCurrentConsumptionConsumptionDataSending),inhabitantData).
classificationOf(return(getCurrentConsumptionCurrentConsumptionDataDisplaying),inhabitantData).
classificationOf(return(getHistoricConsumption),providerData).

classificationOfCall(_AnyParameter,publicData).


% tamperingAbilities(anybody,plombeEntfernen).
% tamperingAbilities(anybody,geraetOeffnen).
% tamperingAbilities(anybody,wpa2Knacken).
% tamperingAbilities(burglar,plombeEntfernen).
% tamperingAbilities(burglar,geraetOeffnen).
% tamperingAbilities(burglar,wpa2Knacken).

locationsAccessibleBy(guest,outdoors).
locationsAccessibleBy(guest,public).
locationsAccessibleBy(guest,attended).
locationsAccessibleBy(handyMan,outdoors).
locationsAccessibleBy(handyMan,public).
locationsAccessibleBy(handyMan,unattended).
locationsAccessibleBy(anybody,outdoors).
locationsAccessibleBy(anybody,public).
locationsAccessibleBy(burglar,outdoors).
locationsAccessibleBy(burglar,public).
locationsAccessibleBy(burglar,unattended).
locationsAccessibleBy(blindDeafGuy,outdoors).
locationsAccessibleBy(blindDeafGuy,public).


containerSecuredByMethod(digitalMeterContainer,plombeEntfernen).
containerSecuredByMethod(tabletContainer,geraetOeffnen).
containerSecuredByMethod(controllerContainer,plombeEntfernen).


furtherConnections(tabletContainer,possible).
furtherConnections(controllerContainer,possible).
furtherConnections(digitalMeterContainer,possible).

sharing(tabletContainer,openShared).
sharing(controllerContainer,controlledExclusive).
sharing(digitalMeterContainer,controlledExclusive).

location(tabletContainer,attended).
location(controllerContainer,unattended).
location(digitalMeterContainer,unattended).


linkLocation(linkMeterController,unattended).
linkLocation(linkControllerTablet,outdoors).

isEncrypted(linkControllerTablet).





exposesPhsicallyAccessiblePayloadTo(Link,Attacker) :-
        attacker(Attacker),
        not(isEncrypted(Link)).

exposesPhsicallyAccessibleMetaDataTo(_Link,Attacker) :-
        attacker(Attacker).


accessibleParameters(Attacker,Parameter) :-
  % Ausgabe-Parameter, auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat
  interfacesAllowedToBeUsedBy(Attacker,Interface),
  systemProvides(Interface),
  services(Interface,Service),
  outputParameters(Service, Parameter).

accessibleParameters(Attacker,Parameter) :-
  % Eingabe-Parameter,auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat, weil er vom System Aufgerufen wird.
  interfacesAllowedToBeUsedBy(Attacker, Interface),
  systemRequires(Interface),
  services(Interface,Service),
  inputParameters(Service, Parameter).

accessibleParameters(Attacker,Parameter) :-
  % Parameter, auf die der Angreifer Zugriff hat, weil er einen entsprechenden ResourceContainer angreifen konnte.
  containersFullyAccessibleBy(Attacker, Container),
  interfacesOn(Container,Interface),
  services(Interface,Service),
  parameters(Service,Parameter).

accessibleParameters(Attacker,Parameter) :-
  % Parameter, auf die der Angreifer Zugriff hat, weil er eine entsprechende LinkResource angreifen konnte.
  linksPayloadFullyAccessibleBy(Attacker, Link),
  linkBetween(Link,ContainerLeft,ContainerRight),
  runsOn(Left,ContainerLeft),
  componentOf(Left,Component),
  requires(Component,Interface),
  systemAssembledTo(Left,Interface,byAssembly(Right)),
  runsOn(Right,ContainerRight),
  services(Interface, Service),
  parameters(Service,Parameter).


observableServices(Attacker, Service) :-
  % Services, deren Aufrufe der Angreifer als regulärer "Benutzer" des Systems beobachten kann
  interfacesAllowedToBeUsedBy(Attacker,Interface),
  systemProvides(Interface),
  services(Interface, Service).

observableServices(Attacker,Service) :-
  % Services, deren Aufrufe der Angreifer beobachten kann, weil er einen entsprechenden ResourceContainer angreifen konnte.
  containersFullyAccessibleBy(Attacker,Container),
  interfacesOn(Container,Interface),
  services(Interface,Service).

observableServices(Attacker, Service) :-
  % Services, deren Aufrufe der Angreifer beobachten kann, weil er eine entsprechende LinkResource angreifen konnte.
  linksMetaDataFullyAccessibleBy(Attacker,Link),
  linkBetween(Link,ContainerLeft, ContainerRight),
  runsOn(Left,ContainerLeft),
  componentOf(Left,Component),
  requires(Component,Interface),
  systemAssembledTo(Left,Interface,byAssembly(Right)),
  runsOn(Right,ContainerRight),
  services(Interface,Service).


dataAccessibleTo(Attacker,DataSet) :-
    accessibleParameters(Attacker,Parameter),
    classificationOf(Parameter,DataSet).

dataAccessibleTo(Attacker,DataSet) :-
    observableServices(Attacker,Service),
    classificationOfCall(Service,DataSet).



isInSecureWithRespectTo(Attacker) :-
        dataAccessibleTo(Attacker,Accessed),
        not(dataAllowedToBeAccessedBy(Attacker,Accessed)).


containersFullyAccessibleBy(Attacker,Container) :-
        containersPhysicalAccessibleBy(Attacker,Container),
        containerSecuredByMethod(Container,Method),
        tamperingAbilities(Attacker,Method).
containersFullyAccessibleBy(_,Container) :-
        sharing(Container,openShared),
        furtherConnections(Container,existing).
containersFullyAccessibleBy(Attacker,Container) :-
        containersPhysicalAccessibleBy(Attacker,Container),
        sharing(Container,openShared),
        furtherConnections(Container,possible).


linksPayloadFullyAccessibleBy(Attacker,Link) :-
    linksPhysicalAccessibleBy(Attacker,Link),
    exposesPhsicallyAccessiblePayloadTo(Link,Attacker).

linksMetaDataFullyAccessibleBy(Attacker,Link) :-
    linksPhysicalAccessibleBy(Attacker,Link),
    exposesPhsicallyAccessibleMetaDataTo(Link,Attacker).


linksPhysicalAccessibleBy(Attacker,Link) :-
    linkLocation(Link,Location),
    locationsAccessibleBy(Attacker,Location).

containersPhysicalAccessibleBy(Attacker,Container) :-
    location(Container,Location),
    locationsAccessibleBy(Attacker,Location).


providedInterfacesOn(Container,Interface) :-
        systemModel(AssemblyContext),
        runsOn(AssemblyContext,Container),
        componentOf(AssemblyContext,Component),
        provides(Component,Interface).

requiredInterfacesOn(Container,Interface) :-
        systemModel(AssemblyContext),
        runsOn(AssemblyContext,Container),
        componentOf(AssemblyContext,Component),
        requires(Component,Interface).


interfacesOn(Container,Interface) :-
        requiredInterfacesOn(Container,Interface).
interfacesOn(Container,Interface) :-
        providedInterfacesOn(Container,Interface).


parameters(Service, Parameter) :-
        inputParameters(Service, Parameter).
parameters(Service, Parameter) :-
        outputParameters(Service, Parameter).

providedInterfaces(Interface) :-
        systemModel(AssemblyContext),
        componentOf(AssemblyContext,Component),
        provides(Component,Interface).


requiredInterfaces(Interface) :-
        systemModel(AssemblyContext),
        componentOf(AssemblyContext,Component),
        requires(Component,Interface).




abducible_predicate(tamperingAbilities/2).
% abducible_predicate(locationsAccessibleBy/2).


ic:- false.

query6(Attacker) :- attacker(Attacker), isInSecureWithRespectTo(Attacker).

nojustify6:-
        write('justify6:'),nl,
        bagof((Attacker), query6(Attacker), Bag),
        length(Bag,L),
        write(Bag),nl,
        write('length:'), write(L), nl.
justify6 :-
        write('justify6:'),nl,
        bagof((Attacker,E),
        just_true(query6(Attacker),E),Bag),
        length(Bag,L),
        write(Bag),nl,
        write('length:'), write(L), nl.
