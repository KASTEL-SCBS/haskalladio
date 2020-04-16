#! /usr/bin/python3

"""
Parses the queries-justify.result.json and parses it into basic data structures
"""
import os
import sys
import json
from dataclasses import dataclass
from typing import List, Dict, Optional

from oquery import OQuery

try:
    from prettyprinter import cpprint, prettyprinter

    pprint = cpprint

    prettyprinter.install_extras(
        include=[
            'dataclasses',
        ],
        warn_on_error=True
    )
except:
    from pprint import pprint

FILE = os.path.join(os.path.dirname(__file__), "queries-justify.result.json")


@dataclass
class Id:

    @staticmethod
    def from_arr(l: list) -> Optional[str]:
        """
         [
                                    "adversary",
                                    [
                                        "30",
                                        "cloud service administrator"
                                    ]
                                ]
        or
        [
                                    "provided",
                                    [
                                        [
                                            "operationInterface",
                                            [
                                                "26",
                                                "FileManGUI"
                                            ]
                                        ]
                                    ]
                                ]
        """
        try:
            if l[0] == "list":
                name = "".join(chr(int(c)) for c in l[1])
                return name
            _, name = int(l[1][0]), l[1][1]
            return name
        except:
            try:
                return Id.from_arr(l[1][0])
            except:
                return None


def ids(ls: list) -> List[Id]:
    return [Id.from_arr(l) for l in ls if Id.from_arr(l) is not None]


class Conclusion:

    @classmethod
    def _get_conclusion_d(cls, d: dict) -> dict:
        """

        :return: "conclusion": [
                    "isInSecureWithRespectTo",
                    [
                        [
                            "adversary",
                            [
                                "30",
                                "cloud service administrator"
                            ]
                        ],
                        …
                       ]
                ],
        """
        return d[1]["conclusion"]

    @classmethod
    def _get_conclusion_name(cls, d: dict) -> str:
        return cls._get_conclusion_d(d)[0]


@dataclass
class ObservableService:
    adversary: str
    operation_signature: str
    assembly_context: str
    provided_operation_interface: str


@dataclass
class AccessibleParameter:
    adversary: str
    name: str
    operation_signature: str
    assembly_context: str
    required_interface: str


@dataclass
class AdversaryInterfaceTuple:
    adversary: str
    operation_interface: str
    assembly_context: str


@dataclass
class ProvidedInterface:
    resource_container: str
    operation_interface: str
    assembly_context: str


@dataclass
class Service:
    operation_signature: str
    assembly_context: str
    provided_interface: str


@dataclass
class Call:
    operation_signature: str
    is_return: bool  # is operation signature prefixed by "return"
    name: Optional[str]  # if is_return == True
    dataset_map_entry: str
    assembly_context: str
    provided_interface: str  # operation interface
    not_may_know_dataset_map_entry: str
    may_know_data_set_map_entry: List[Id]

    @classmethod
    def from_dicts(cls, l: dict, k: dict) -> 'Call':
        op, d, ret, name = None, None, None, None
        try:
            op, d = ids(l["conclusion"][1][0:2])
            ret = False
        except:
            name = l["conclusion"][1][0][0]
            op = Id.from_arr(l["conclusion"][1][0][1][0][1][0])
            d = Id.from_arr(l["conclusion"][1][2])
            ret = True
        return Call(op, ret, name, d, Id.from_arr(l["conclusion"][1][2][1][0]),
                    Id.from_arr(l["conclusion"][1][2][1][1]),
                    Id.from_arr(k["conclusion"]["not"][1][1]),
                    ids(k["premises"][0][1][1][1]))


@dataclass
class ServiceNotAllowedToBeObservedBy:
    service: Service
    included_calls: List[Call]

    @classmethod
    def from_dict(cls, d: dict) -> 'ServiceNotAllowedToBeObservedBy':
        service = Service(Id.from_arr(d["conclusion"]["not"][1][1]), *ids(d["conclusion"]["not"][1][2][1]))
        prems = d["premises"]
        calls = []
        for i in range(0, len(prems), 2):
            lookup = prems[i]
            knows = prems[i + 1]
            calls.append(Call.from_dicts(lookup, knows))
        return ServiceNotAllowedToBeObservedBy(service, calls)

@dataclass
class ContainerPhysicalAccessibleBy:

    adversary: str
    resourceContainer: str
    location: str


@dataclass
class Location:

    resourceCountainer: str
    location: str
    tamperProtection: str


@dataclass
class Interface:

    resourceContainer: str
    operationInterface: str
    assemblyContext: str
    basicComponent: str


@dataclass
class SecureWithRespectToConclusion(Conclusion):
    observable_service: Optional[ObservableService]
    # both exclude each other
    accessible_parameter: Optional[AccessibleParameter]
    requiredInterfacesAccessibleTo: AdversaryInterfaceTuple
    containerPhysicalAccessibleBy: ContainerPhysicalAccessibleBy
    location: Location
    requiredInterfacesOn: Interface
    with_respect_to_service: Service
    service_forbidden_to_be_observed_by: Service
    service_not_allowed_to_be_observed_by: ServiceNotAllowedToBeObservedBy

    @classmethod
    def from_dict(cls, d: dict) -> 'SecureWithRespectToConclusion':
        """
          "premises": [
            [
                "attacker",
                …
            ],
            {
                "conclusion": [
                    "isInSecureWithRespectTo",
        """
        obs = d[1]["premises"][0]
        observable_service = None
        accessible_parameter = None
        try:
            observable_service = ObservableService(*ids(obs["conclusion"][1]))
        except:
            accessible_parameter = AccessibleParameter(adversary=Id.from_arr(obs["conclusion"][1][0]),
                                                       name=obs["conclusion"][1][1][0],
                                                       operation_signature=Id.from_arr(obs["conclusion"][1][1][1][0]),
                                                       assembly_context=Id.from_arr(obs["conclusion"][1][2]),
                                                       required_interface=Id.from_arr(obs["conclusion"][1][3]))
        requiredInterfacesAccessibleTo = AdversaryInterfaceTuple(*ids(obs["premises"][0]["conclusion"][1]))
        containerPhysicalAccessibleBy = ContainerPhysicalAccessibleBy(*ids(obs["premises"][0]["premises"][0]["conclusion"][1]))
        location = Location(*ids(obs["premises"][0]["premises"][0]["premises"][0][1]))
        requiredInterfacesOn = Interface(*ids(obs["premises"][0]["premises"][1]["conclusion"][1]), Id.from_arr(obs["premises"][0]["premises"][1]["premises"][0][1][0][1]))

        wrts = d[1]["premises"][1]
        wrt = Service(Id.from_arr(wrts["conclusion"][1][1]),
                      *ids(wrts["conclusion"][1][2][1]))
        wrt_prem = wrts["premises"][0]
        service_forbidden_to_be_obs_by = Service(Id.from_arr(wrt_prem["conclusion"][1][1]),
                                                 *ids(wrt_prem["conclusion"][1][2][1]))
        wrt_prem2 = wrt_prem["premises"][0]
        return SecureWithRespectToConclusion(observable_service, accessible_parameter, requiredInterfacesAccessibleTo,
                                             containerPhysicalAccessibleBy,
                                             location, requiredInterfacesOn, wrt,
                                             service_forbidden_to_be_obs_by,
                                             ServiceNotAllowedToBeObservedBy.from_dict(wrt_prem2))

    @classmethod
    def is_applicable(cls, d: dict) -> bool:
        """ see above """
        return cls._get_conclusion_name(d) == "isInSecureWithRespectTo"


@dataclass
class World:
    conclusions_per_attacker: Dict[Id, List[SecureWithRespectToConclusion]]

    @staticmethod
    def from_arr(arr: List[dict]) -> 'World':
        return World({World._attacker(d): SecureWithRespectToConclusion.from_dict(d["premises"])
                      for d in arr if SecureWithRespectToConclusion.is_applicable(d["premises"])})

    @staticmethod
    def _attacker(d: dict) -> Id:
        return Id.from_arr(d["conclusion"])


def conv_list(l: OQuery) -> str:
    return "".join(chr(int(c.obj)) for c in l[1])


def parse(file: str) -> World:
    with open(file, "r") as f:
        d = json.loads(f.read())
        o = OQuery(d)
        #res = o.not_all(lambda x: x[0] == "isInSecureWithRespectTo").all(lambda x: x[0] == "list", conv_list)
        #print(res)
        return World.from_arr(d)


if __name__ == '__main__':
    pprint(parse(sys.argv[1] if len(sys.argv) > 1 else FILE))
