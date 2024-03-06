from typing import Iterator, Any, Callable, Optional

from TrueOrFalseBecause import BoolBecause, TrueBecause, FalseBecause, forallM


class Magma:

    def gen(self) -> Iterator[Any]:
        raise NotImplemented

    def op(self, a, b) -> Any:
        raise NotImplemented

    def member(self, a) -> BoolBecause:
        raise NotImplemented

    def equiv(self, a, b) -> BoolBecause:
        if a == b:
            return TrueBecause(f"{a} == {b}")
        else:
            return FalseBecause(f"{a} != {b}")

    def isClosed(self) -> BoolBecause:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: self.member(self.op(a, b))
                                         ).mapIfFalse(lambda str: f"not closed because {str}")
                       ) and TrueBecause(f"{self} is closed")

    def isAssociative(self) -> BoolBecause:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: forallM(self.gen(),
                                                           lambda c: self.equiv(self.op(self.op(a, b), c),
                                                                                self.op(a, self.op(b, c))
                                                                                ).mapIfFalse(lambda str: f"not associative: {a}, {b}, {c} ")))
                       ) and TrueBecause(f"{self} is associative")

    def isAbelian(self) -> BoolBecause:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: self.equiv(self.op(a, b),
                                                              self.op(b, a)
                                                              ).mapIfFalse(lambda str: f"not Abelian, e.g., {a},{b}"))
                       ) and TrueBecause(f"{self} is Abelian")

    def isIdentity(self, z) -> BoolBecause:
        comment = f"{z} is not an identity because"
        return forallM(self.gen(),
                       lambda a: (self.equiv(self.op(z, a), a
                                             ).mapIfFalse(lambda str: f"{comment} op({a},{a}) = {self.op(z, a)}")
                         and self.equiv(self.op(a, z), a
                                        ).mapIfFalse(lambda str: f"{comment} op({a},{z}) = {self.op(a, z)}"))
                       ) and TrueBecause(f"{z} is the identity")

    def findIdentity(self):
        for z in self.gen():
            if self.isIdentity(z):
                return z
        return None

    def findInverse2(self, z, a):
        return next((b for b in self.gen() if self.equiv(z, self.op(a, b)) and self.equiv(z, self.op(b, a))),
                    None)

    def findInverse1(self, a):
        z = self.findIdentity()
        if z is None:
            return None
        return self.findInverse2(z, a)

    def isInverter(self, z, invert: Callable[[Any], Optional[Any]]) -> BoolBecause:
        def f(a) -> BoolBecause:
            b = invert(a)
            if b is None:
                return FalseBecause(f"{a} has no inverse")
            return self.member(b) and self.equiv(z, self.op(a, b)) and self.equiv(z, self.op(b, a))

        return forallM(self.gen(), f) and TrueBecause(f"{self} is invertible")

    def isSemiGroup(self) -> BoolBecause:
        return (self.isClosed() and
                self.isAssociative() and
                TrueBecause(f"{self} is a semigroup")
                ).ifFalse(lambda str: FalseBecause(f"{self} is not a semigroup because {str}"))

    def isMonoid(self, z) -> BoolBecause:
        return (self.isSemiGroup() and
                self.isIdentity(z) and
                TrueBecause(f"{self} is a monoid")
                ).mapIfFalse(lambda str: f"{self} is not a monoid because {str}")

    def isGroup(self, z, invert) -> BoolBecause:
        return (self.isMonoid(z) and
                self.isInverter(z, invert)
                and TrueBecause(f"{self} is a group")
                ).mapIfFalse(lambda str: f"{self} is not a group because {str}")


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

    def member(self, a) -> BoolBecause:
        if self.member1(a):
            return TrueBecause(f"{a} is a member")
        else:
            return FalseBecause(f"{a} is not a member")


class ModP (Magma):
    def __init__(self, p: int):
        self.p = p

    def __repr__(self):
        return f"ModP({self}.p"

    def gen(self) -> Iterator[int]:
        return range(self.p)

    def equiv(self, a: int, b: int) -> BoolBecause:
        if a == b:
            return TrueBecause(f"{a} equiv {b}")
        else:
            return FalseBecause(f"{a} not equiv {b}")

    def member(self, a: int) -> BoolBecause:
        if a < 0:
            return FalseBecause(f"{a} is not a member because {a}<0")
        elif a >= self.p:
            return FalseBecause(f"{a} is not a member because {a}>={self.p}")
        else:
            return TrueBecause(f"0 <= {a} < {self.p}")


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


def genFinite(n: int) -> Iterator[int]:
    for i in range(n):
        yield i


def cayleyTable(elements, dyn_op) -> str:
    header = '*|' + ' '.join(f"{i}" for i in elements)
    divider = '-+' + '-'.join('-' for _ in elements)

    def row(x):
        return f"{x}|" + ' '.join(f"{dyn_op(x,y)}" for y in elements)

    return '\n' + header + '\n' + divider + '\n' + '\n'.join(row(x) for x in elements)


def testModP():
    for p in range(2, 12):
        add = AdditionModP(p)
        mult = MultiplicationModP(p)

        assert add.isClosed()
        assert add.isSemiGroup()
        assert add.isMonoid(0)
        print(add.isGroup(0, lambda a: (p-a) % p).because)

        def invert(a):
            return next((b for b in range(1, p) if (a*b) % p == 1), None)

        mig = mult.isGroup(1, invert)
        # print(cayleyTable(range(1, p), mult.op))

        print(mig.because)
