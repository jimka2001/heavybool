from typing import Callable, Any, TypeVar, Iterable, Optional, Tuple, Generator


class HeavyBool:
    def __init__(self, parity, because=[]):
        if parity:
            self.__class__ = HeavyTrue
        else:
            self.__class__ = HeavyFalse
        if not because:
            self.because = []
        elif isinstance(because, list):
            for m in because:
                assert isinstance(m, dict)
            self.because = because
        elif isinstance(because, dict):
            self.because = [because]
        elif isinstance(because, str):
            self.because = [{'reason': because}]
        else:
            raise RuntimeError(f"invalid type of {because=} type={type(because)}")

    def __eq__(self, other):
        return other.because == self.because

    def Not(self) -> 'HeavyBool':
        raise NotImplementedError  # implemented in subclass

    def __add__(self, other) -> 'HeavyBool':
        raise NotImplementedError  # implemented in subclass

    def __bool__(self) -> 'HeavyBool':
        raise NotImplementedError  # implemented in subclass

    def annotate(self, because: dict):
        assert isinstance(because, dict)
        return self + because

    def annotateTrue(self, because: dict) -> 'HeavyBool':
        return self

    def annotateFalse(self, because: dict) -> 'HeavyBool':
        return self

    def flag(self, key: str) -> 'HeavyBool':
        return self + {key: self.__bool__()}


class HeavyTrue(HeavyBool):
    def __init__(self, because=[]):
        super().__init__(True, because)

    def __eq__(self, other):
        return isinstance(other, HeavyTrue) and super().__eq__(other)

    def __repr__(self):
        return f"True[{self.because}]"

    def __bool__(self):
        return True

    def Not(self):
        return HeavyFalse(self.because)

    def __add__(self, because):
        assert isinstance(because, dict)
        return HeavyTrue(self.because + [because])

    def annotateTrue(self, because: dict):
        if self:
            return self.annotate(because)
        else:
            return self


class HeavyFalse(HeavyBool):
    def __init__(self, because=[]):
        super().__init__(False, because)

    def __eq__(self, other):
        return isinstance(other, HeavyTrue) and super().__eq__(other)

    def __repr__(self):
        return f"False[{self.because}]"

    def __bool__(self):
        return False

    def __add__(self, because) -> 'HeavyBool':
        assert isinstance(because, dict)
        return HeavyFalse(self.because + [because])

    def Not(self):
        return HeavyTrue(self.because)

    def annotateFalse(self, because: dict):
        if self:
            return self
        else:
            return self.annotate(because)


def existsM(items, p: Callable[[Any], HeavyBool]) -> HeavyBool:
    for i in items:
        r = p(i)
        if r:
            return HeavyTrue({"witness": i})

    return HeavyFalse()


def forallM(items, p: Callable[[Any], HeavyBool]) -> HeavyBool:
    return existsM(items, lambda i: p(i).Not()).Not()


def anyM(gen: Generator[HeavyBool, None, None]) -> HeavyBool:
    for hb in gen:
        if hb:
            return hb
    return HeavyFalse()


def allM(gen: Generator[HeavyBool, None, None]) -> HeavyBool:
    for hb in gen:
        if not hb:
            return hb
    return HeavyTrue()
