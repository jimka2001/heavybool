from typing import Callable, Any, TypeVar, Iterable, Optional, Tuple


class ModP (Magma):
    def __init__(self, p: int):
        self.p = p

    def __repr__(self):
        return f"ModP({self}.p"

    def gen(self) -> Iterator[int]:
        return range(self.p)

    def equiv(self, a: int, b: int) -> HeavyBool:
        if a == b:
            return HeavyTrue(f"{a} equiv {b}")
        else:
            return HeavyFalse(f"{a} not equiv {b}")

    def member(self, a: int) -> HeavyBool:
        if a < 0:
            return HeavyFalse(f"{a} is not a member because {a}<0")
        elif a >= self.p:
            return HeavyFalse(f"{a} is not a member because {a}>={self.p}")
        else:
            return HeavyTrue(f"0 <= {a} < {self.p}")


class AdditionModP (ModP):
    def __repr__(self):
        return f"AdditionModP({self.p})"

    def op(self, a: int, b: int) -> int:
        return (a+b) % self.p


class MultiplicationModP(ModP):
    def __repr__(self):
        return f"MultiplicationModP({self.p})"

    def gen(self) -> Iterator[int]:
        for i in super().gen():
            if i != 0:
                yield i

    def op(self, a: int, b: int) -> int:
        return (a * b) % self.p

