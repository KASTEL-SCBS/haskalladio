"""
Simple library to allow querying complex dictionary and list data structures, disregarding
the actual hierarchy

Requires currently non circular data structures and works best with dict, list and basic types

dict → list of key,value-pairs
list → list of items
"""
import itertools
from dataclasses import dataclass
from typing import Callable, Any, Iterator, Optional

Filter = Callable[['OQuery'], bool]
Processor = Callable[['OQuery'], Any]


@dataclass(frozen=True)
class OQuery:

    obj: Any

    @classmethod
    def _wrap(cls, obj: Any) -> "OQuery":
        if isinstance(obj, OQuery):
            return obj
        return OQuery(obj)

    @classmethod
    def _unwrap(cls, obj: Any) -> Any:
        if isinstance(obj, OQuery):
            return obj.obj
        return obj

    def _all_non_rec(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None) -> Iterator["OQuery"]:
        """
        Returns all wrapped elements that match the filter. Errors thrown by the filter
        or the processor mean that the object is not considered.
        """
        filter = filter or (lambda x: True)
        processor = processor or (lambda x: self._unwrap(x))
        it = None
        if isinstance(self.obj, list):
            it = iter(self.obj)
        elif isinstance(self.obj, dict):
            it = iter(self.obj.items())
        for item in it:
            try:
                if filter(item):
                    yield self._wrap(processor(self._wrap(item)))
            except:
                pass

    def _all_rec(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None) -> Iterator["OQuery"]:
        """
        Returns all wrapped elements that match the filter. Errors thrown by the filter
        or the processor mean that the object is not considered.
        If an object is considered, then all its sub objects are ignored

        Recursive version
        """
        filter = filter or (lambda x: True)
        processor = processor or (lambda x: self._unwrap(x))
        it = None
        if isinstance(self.obj, list) or isinstance(self.obj, tuple):
            it = iter(self.obj)
        elif isinstance(self.obj, dict):
            it = iter(self.obj.items())
        else:
            return
        for item in it:
            accepted = False
            wrapped = self._wrap(item)
            try:
                if filter(wrapped):
                    yield self._wrap(processor(wrapped))
                    accepted = True
            except:
                pass
            if not accepted:
                for sub in wrapped._all_rec(filter, processor):
                    yield sub

    def _all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True):
        for item in (self._all_rec(filter, processor) if recursive else self._all_non_rec(filter, processor)):
            yield item

    def all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True) -> "OQuery":
        return OQuery(list(self._all(filter, processor, recursive)))

    def not_all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True) -> "OQuery":
        if filter is None:
            return
        def fil(x):
            try:
                return not filter(x)
            except:
                return True
        return self.all(fil, processor, recursive)

    def first(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True) -> "OQuery":
        return next(self._all(filter, processor, recursive))

    def __getitem__(self, item) -> "OQuery":
        return self._wrap(self.obj[item])

    def __iter__(self) -> Iterator["OQuery"]:
        for sub in self.obj:
            yield self._wrap(sub)

    def __reversed__(self) -> Iterator["OQuery"]:
        for sub in reversed(self.obj):
            yield self._wrap(sub)

    def __contains__(self, item) -> bool:
        return self._unwrap(item) in self.obj

    def __hash__(self):
        return hash(self.obj)

    def __eq__(self, other):
        return self._unwrap(other) == self.obj

    def __len__(self):
        return len(self.obj)

    def __str__(self):
        return str(self.obj)

    def __repr__(self):
        return repr(self.obj)