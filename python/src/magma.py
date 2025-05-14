from typing import Iterator, Any, Callable, Optional

from src.heavybool import HeavyBool, HeavyTrue, HeavyFalse, allM


class Magma:

    def gen(self) -> Iterator[Any]:
        raise NotImplemented

    def op(self, a, b) -> Any:
        raise NotImplemented

    def member(self, a) -> HeavyBool:
        raise NotImplemented

    def equiv(self, a, b) -> HeavyBool:
        if a == b:
            return HeavyTrue({"reason": f"{a} == {b}",
                              "a": a,
                              "b": b})
        else:
            return HeavyFalse({"reason": f"{a} != {b}",
                               "a": a,
                               "b": b})

    def isClosed(self) -> HeavyBool:
        return allM(self.member(self.op(a, b)).annotate({"a": a,
                                                         "b": b})
                    for a in self.gen()
                    for b in self.gen()
                    ).flag("closed")

    def isAssociative(self) -> HeavyBool:
        return allM(
            # (a+b) + c == a + (b+c)
            self.equiv(self.op(self.op(a, b), c),
                       self.op(a, self.op(b, c))).annotate({"a": a,
                                                            "b": b,
                                                            "c": c})
            for a in self.gen()
            for b in self.gen()
            for c in self.gen()).flag("associative")

    def isAbelian(self) -> HeavyBool:
        return allM(self.equiv(self.op(a, b),
                               self.op(b, a)).annotate({"a": a, "b": b})
                    for a in self.gen()
                    for b in self.gen()
                    ).flag("abelian")

    def isIdentity(self, z) -> HeavyBool:
        return allM(self.equiv(self.op(z, a), a).annotate({"za": self.op(z, a),
                                                           "a": a}) and
                    self.equiv(self.op(a, z), a).annotate({"az": self.op(a, z),
                                                           "a": a})
                    for a in self.gen()).annotateFalse({"z": z}).flag("identity")

    def findIdentity(self):
        for z in self.gen():
            if self.isIdentity(z):
                return z
        return None

    def findInverse2(self, z, a):
        return next((b for b in self.gen()
                       if self.equiv(z, self.op(a, b))
                          and self.equiv(z, self.op(b, a))),
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
                return HeavyFalse({"has no inverse": a})
            return self.member(b) and self.equiv(z, self.op(a, b)) and self.equiv(z, self.op(b, a))

        return allM(f(a) for a in self.gen()).flag("valid inverter")

    def isSemiGroup(self) -> HeavyBool:
        return (self.isClosed() and
                self.isAssociative()
                ).flag("semigroup")

    def isMonoid(self, z) -> HeavyBool:
        return (self.isSemiGroup() and
                self.isIdentity(z)
                ).flag("monoid")

    def isGroup(self, z, invert) -> HeavyBool:
        return (self.isMonoid(z) and
                self.isInverter(z, invert)
                ).flag("group")


def genFinite(n: int) -> Iterator[int]:
    for i in range(n):
        yield i


def cayleyTable(elements, dyn_op) -> str:
    header = '*|' + ' '.join(f"{i}" for i in elements)
    divider = '-+' + '-'.join('-' for _ in elements)

    def row(x):
        return f"{x}|" + ' '.join(f"{dyn_op(x, y)}" for y in elements)

    return '\n' + header + '\n' + divider + '\n' + '\n'.join(row(x) for x in elements)
