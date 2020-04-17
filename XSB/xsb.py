#! /usr/bin/python3

"""
Parses the queries-justify.result.json and parses it into basic data structures
"""
import dataclasses
import os
import sys
import json
from abc import ABC
from dataclasses import dataclass
from enum import Enum
from typing import List, Dict, Optional, Any, cast

from oquery import OQuery

from fn import _

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


class Mode(Enum):

    RETURN = 1
    PARAMETER = 2
    SERVICE = 3


id_conv = _[0] == "list", lambda x: "".join(chr(int(c.obj)) for c in x[1])


@dataclass
class AbstractProofObjectClass:
    """ Abstract class for proofs, used to construct generic objects """

    @classmethod
    def from_oquery(cls, o: OQuery) -> "AbstractProofObjectClass":
        """ For each field f: look for [field_name, id] and use the id as its value """
        ps: Dict[str, str] = {}
        for f in dataclasses.fields(cls):
            query = _[0] == f.name, lambda x: x.first(*id_conv)
            try:
                ps[f.name] = cls._from_oquery(o, f.name)
            except:
                if f.type is Optional:
                    ps[f.name] = (o.maybe_first(*query) or OQuery(None)).obj
                else:
                    ps[f.name] = o.first(*query).obj
        return cls(**ps)

    @classmethod
    def _from_oquery(cls, o: OQuery, attribute: str) -> Any:
        raise NotImplementedError()


@dataclass
class World:
    proofs: List[AbstractProofObjectClass]

    @staticmethod
    def _single_from_oquery(o: OQuery) -> "IsInSecureInRespectToService":
        """ Turns the passed object into a specific sub class """
        return cast(IsInSecureInRespectToService,
                    (IsInSecureInRespectToObservableParameters
                     if o.any(_ == "accessibleParameters") else
                     IsInSecureInRespectToObservableService)
                    .from_oquery(o))

    @staticmethod
    def from_obj(obj: Any) -> 'World':
        return World(OQuery(obj).all(_["conclusion"][0] == "isInSecureWithRespectTo").map_all(World._single_from_oquery))


@dataclass
class IsInSecureInRespectToService(AbstractProofObjectClass, ABC):

    adversary: str
    operationSignature: str
    assemblyContext: str
    required: str  # required interface
    resourceContainer: str
    location: str

    def mode(self) -> Mode:
        raise NotImplementedError()


@dataclass
class IsInSecureInRespectToObservableService(IsInSecureInRespectToService):

    def mode(self) -> Mode:
        return Mode.SERVICE

    @classmethod
    def _from_oquery(cls, o: OQuery, attribute: str) -> Any:
        ret = o.first(_[0] == "observableServices").all(*id_conv)
        attrs = ["adversary", "operationSignature", "assemblyContext", "required"]
        if attribute in attrs:
            return ret[attrs.index(attribute)].obj
        return o.first(_[0] == "location").all(*id_conv)[["resourceContainer", "location"].index(attribute)].obj


@dataclass
class IsInSecureInRespectToObservableParameters(IsInSecureInRespectToService):
    parameter: Optional[str]

    def mode(self) -> Mode:
        return Mode.RETURN if self.parameter is None else Mode.PARAMETER

    @classmethod
    def _from_oquery(cls, o: OQuery, attribute: str) -> Any:
        ret = o.first(_[0] == "accessibleParameters").all(*id_conv)
        attrs = ["adversary", "operationSignature"] + (["parameter"] if not ret.any(_ == "return") else []) + ["assemblyContext", "required"]
        if attribute in attrs:
            return ret[attrs.index(attribute)].obj
        return o.first(_[0] == "location").all(*id_conv)[["resourceContainer", "location"].index(attribute)].obj


def parse(file: str) -> World:
    with open(file, "r") as f:
        d = json.loads(f.read())
        return World.from_obj(d)


if __name__ == '__main__':
    pprint(parse(sys.argv[1] if len(sys.argv) > 1 else FILE))
