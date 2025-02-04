from typing import Iterator, Any, Callable, Optional


class Magma_basic:

    def gen(self) -> Iterator[Any]:
        raise NotImplemented

    def op(self, a, b) -> Any:
        raise NotImplemented

    def member(self, a) -> bool:
        raise NotImplemented

    def isClosed(self) -> bool:
        return all(self.member(self.op(a, b))
                   for a in self.gen()
                   for b in self.gen())

    def isAssociative(self) -> bool:
        return all(self.op(self.op(a, b), c) == self.op(a, self.op(b, c))
                   for a in self.gen()
                   for b in self.gen()
                   for c in self.gen())

    def isAbelian(self) -> bool:
        return all(self.op(a, b) == self.op(b, a)
                   for a in self.gen()
                   for b in self.gen())

    def isIdentity(self, z) -> bool:
        return all(a == self.op(z, a) == self.op(a, z)
                   for a in self.gen())

    def hasIdentity(self) -> bool:
        return any(self.isIdentity(z)
                   for z in self.gen)

    def findIdentity(self):
        return next((z for z in self.gen()
                     if self.isIdentity(z)),
                    None)

    def hasInverses(self, z) -> bool:
        z = self.findIdentity()
        # \forall a \in G \exists b \in G, z = a*b = b*a
        return all(any(z == self.op(a, b) == self.op(b, a)
                       for b in self.gen())
                   for a in self.gen())

    def isSemiGroup(self) -> bool:
        return (self.isClosed() and
                self.isAssociative())

    def isMonoid(self) -> bool:
        return (self.isSemiGroup() and
                self.hasIdentity())

    def isGroup(self) -> bool:
        return (self.isMonoid() and
                self.hasInverses())
