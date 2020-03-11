"""
Parses the queries-justify.result.json and parses it into basic data structures
"""
import os
import sys
import json
from dataclasses import dataclass
from typing import List, Any, Dict, Optional, Union

FILE = os.path.dirname(__file__) + "/queries-justify.result.json"


@dataclass
class Id:
    id: int
    name: str

    def __hash__(self) -> int:
        return self.id

    def __eq__(self, other) -> bool:
        return isinstance(other, self.__class__) and other.id == id

    @staticmethod
    def from_arr(l: list) -> 'Id':
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
            return Id(int(l[1][0]), l[1][1])
        except:
            try:
                return Id(int(l[1][0][1][0]), l[1][0][1][1])
            except:
                return None

    def __int__(self) -> int:
        return self.id


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
    adversary: Id
    operation_signature: Id
    assembly_context: Id
    provided_operation_interface: Id


@dataclass
class AccessibleParameter:
    adversary: Id
    name: str
    operation_signature: Id
    assembly_context: Id
    required_interface: Id


@dataclass
class AdversaryResourceTuple:
    adversary: Id
    resource_container: Id


@dataclass
class ProvidedInterface:
    resource_container: Id
    operation_interface: Id
    assembly_context: Id


@dataclass
class Service:
    operation_signature: Id
    assembly_context: Id
    provided_interface: Id


@dataclass
class SecureWithRespectToConclusion(Conclusion):
    observable_service: Optional[ObservableService]
    # both exclude each other
    accessible_parameter: Optional[AccessibleParameter]
    containers_fully_accessible_by: AdversaryResourceTuple
    shared_of_resource_container: Id
    further_existing_connection: Id
    provided_interface: ProvidedInterface
    with_respect_to_service: Service
    service_forbidden_to_be_observed_by: Service

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
        containers_fully_accessible_by = AdversaryResourceTuple(*ids(obs["premises"][0]["conclusion"][1]))
        shared_res_container = Id.from_arr(obs["premises"][0]["premises"][0][1][0])
        further_ex_interface = Id.from_arr(obs["premises"][0]["premises"][1][1][0])
        provided_interface = ProvidedInterface(*ids(obs["premises"][1]["conclusion"][1]))
        wrts = d[1]["premises"][1]
        wrt = Service(Id.from_arr(wrts["conclusion"][1][1]),
                      *ids(wrts["conclusion"][1][2][1]))
        wrt_prem = wrts["premises"][0]
        service_forbidden_to_be_obs_by = Service(Id.from_arr(wrt_prem["conclusion"][1][1]),
                                                 *ids(wrt_prem["conclusion"][1][2][1]))
        wrt_prem2 = wrt_prem["premises"][0]
        return SecureWithRespectToConclusion(observable_service, accessible_parameter, containers_fully_accessible_by,
                                             shared_res_container,
                                             further_ex_interface, provided_interface, wrt,
                                             service_forbidden_to_be_obs_by)

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


def parse(file: str) -> World:
    with open(file, "r") as f:
        return World.from_arr(json.loads(f.read()))


if __name__ == '__main__':
    print(repr(parse(sys.argv[1] if len(sys.argv) > 1 else FILE)))
