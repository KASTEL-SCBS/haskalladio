"""
Simple library to allow querying complex dictionary and list data structures, disregarding
the actual hierarchy

Requires currently non circular data structures and works best with dict, list and basic types

dict → list of key,value-pairs
list → list of items
"""

from dataclasses import dataclass
from typing import Callable, Any, Iterator, Optional, Tuple, List

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

    def _single(self, item: Any, filter: Filter = None, processor: Processor = None, catch_processor: bool = True) -> Tuple[bool, Any]:
        wrapped = self._wrap(item)
        use = False
        try:
            use = filter(wrapped)
        except:
            use = False
        if use:
            if catch_processor:
                try:
                    return True, self._wrap(processor(wrapped))
                except:
                    pass
            else:
                return True, self._wrap(processor(wrapped))
        return False, None

    def _all_non_rec(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, catch_processor: bool = True) -> Iterator["OQuery"]:
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
            success, res = self._single(item, filter, processor, catch_processor)
            if success:
                yield res

    def _all_rec(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, catch_processor: bool = False) -> Iterator["OQuery"]:
        """
        Returns all wrapped elements that match the filter. Errors thrown by the filter
        or the processor mean that the object is not considered.
        If an object is considered, then all its sub objects are ignored

        Recursive version
        """
        filter = filter or (lambda x: True)
        processor = processor or (lambda x: self._unwrap(x))
        if isinstance(self.obj, list) or isinstance(self.obj, tuple):
            it = iter(self.obj)
        elif isinstance(self.obj, dict):
            it = iter(self.obj.items())
        else:
            return
        for item in it:
            success, res = self._single(item, filter, processor, catch_processor)
            if not success:  # TODO unroll recursion
                for sub in self._wrap(item)._all_rec(filter, processor, catch_processor):
                    yield sub
            else:
                yield res

    def _all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True, catch_processor: bool = False):
        for item in (self._all_rec(filter, processor, catch_processor) if recursive else self._all_non_rec(filter, processor, catch_processor)):
            yield item

    def all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True, catch_processor: bool = False) -> "OQuery":
        return OQuery(list(self._all(filter, processor, recursive, catch_processor)))

    def not_all(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True, catch_processor: bool = False) -> "OQuery":
        if filter is None:
            return

        def fil(x):
            try:
                return not filter(x)
            except:
                return True

        return self.all(fil, processor, recursive, catch_processor)

    def first(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True, catch_processor: bool = False) -> "OQuery":
        return next(self._all(filter, processor, recursive, catch_processor))

    def maybe_first(self, filter: Optional[Filter] = None, processor: Optional[Processor] = None, recursive: bool = True, catch_processor: bool = False) -> Optional["OQuery"]:
        try:
            return self.first(filter, processor, recursive, catch_processor)
        except:
            return None

    def any(self, filter: Optional[Filter] = None) -> bool:
        return self.maybe_first(filter) is not None

    def map_all(self, processor: Processor) -> List[Any]:
        return list(self._unwrap(item) for item in self._all(processor=processor, catch_processor=False))

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