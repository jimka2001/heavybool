from typing import Callable, Any, TypeVar, Iterable, Optional, Tuple


class DynMagma (Magma):
    def __init__(self,
                 gen1: Callable[[], Iterator[Any]],
                 op1: Callable[[Any, Any], Any],
                 member1: Callable[[Any], bool]):
        self.gen1 = gen1
        self.op1 = op1
        self.member1 = member1

    def __repr__(self):
        return "dyn"

    def gen(self) -> Iterator[Any]:
        for i in self.gen1():
            yield i

    def op(self, a, b) -> Any:
        return self.op1(a, b)

    def member(self, a) -> HeavyBool:
        if self.member1(a):
            return HeavyTrue(f"{a} is a member")
        else:
            return HeavyFalse(f"{a} is not a member")

