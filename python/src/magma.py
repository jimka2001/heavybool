from typing import Iterator, Any, Callable, Optional

from heavybool import HeavyBool, HeavyTrue, FalseBecause, forallM


class Magma:

    def gen(self) -> Iterator[Any]:
        raise NotImplemented

    def op(self, a, b) -> Any:
        raise NotImplemented

    def member(self, a) -> HeavyBool:
        raise NotImplemented

    def equiv(self, a, b) -> HeavyBool:
        if a == b:
            return HeavyTrue(f"{a} == {b}")
        else:
            return HeavyFalse(f"{a} != {b}")

    def isClosed(self) -> HeavyBool:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: self.member(self.op(a, b))
                                         ).mapIfFalse(lambda str: f"not closed because {str}")
                       ) and HeavyTrue(f"{self} is closed")

    def isAssociative(self) -> HeavyBool:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: forallM(self.gen(),
                                                           lambda c: self.equiv(self.op(self.op(a, b), c),
                                                                                self.op(a, self.op(b, c))
                                                                                ).mapIfFalse(lambda str: f"not associative: {a}, {b}, {c} ")))
                       ) and HeavyTrue(f"{self} is associative")

    def isAbelian(self) -> HeavyBool:
        return forallM(self.gen(),
                       lambda a: forallM(self.gen(),
                                         lambda b: self.equiv(self.op(a, b),
                                                              self.op(b, a)
                                                              ).mapIfFalse(lambda str: f"not Abelian, e.g., {a},{b}"))
                       ) and HeavyTrue(f"{self} is Abelian")

    def isIdentity(self, z) -> HeavyBool:
        comment = f"{z} is not an identity because"
        return forallM(self.gen(),
                       lambda a: (self.equiv(self.op(z, a), a
                                             ).mapIfFalse(lambda str: f"{comment} op({a},{a}) = {self.op(z, a)}")
                         and self.equiv(self.op(a, z), a
                                        ).mapIfFalse(lambda str: f"{comment} op({a},{z}) = {self.op(a, z)}"))
                       ) and HeavyTrue(f"{z} is the identity")

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

    def isInverter(self, z, invert: Callable[[Any], Optional[Any]]) -> HeavyBool:
        def f(a) -> HeavyBool:
            b = invert(a)
            if b is None:
                return HeavyFalse(f"{a} has no inverse")
            return self.member(b) and self.equiv(z, self.op(a, b)) and self.equiv(z, self.op(b, a))

        return forallM(self.gen(), f) and HeavyTrue(f"{self} is invertible")

    def isSemiGroup(self) -> HeavyBool:
        return (self.isClosed() and
                self.isAssociative() and
                HeavyTrue(f"{self} is a semigroup")
                ).ifFalse(lambda str: HeavyFalse(f"{self} is not a semigroup because {str}"))

    def isMonoid(self, z) -> HeavyBool:
        return (self.isSemiGroup() and
                self.isIdentity(z) and
                HeavyTrue(f"{self} is a monoid")
                ).mapIfFalse(lambda str: f"{self} is not a monoid because {str}")

    def isGroup(self, z, invert) -> HeavyBool:
        return (self.isMonoid(z) and
                self.isInverter(z, invert)
                and HeavyTrue(f"{self} is a group")
                ).mapIfFalse(lambda str: f"{self} is not a group because {str}")


def genFinite(n: int) -> Iterator[int]:
    for i in range(n):
        yield i


def cayleyTable(elements, dyn_op) -> str:
    header = '*|' + ' '.join(f"{i}" for i in elements)
    divider = '-+' + '-'.join('-' for _ in elements)

    def row(x):
        return f"{x}|" + ' '.join(f"{dyn_op(x,y)}" for y in elements)

    return '\n' + header + '\n' + divider + '\n' + '\n'.join(row(x) for x in elements)


