from src.modp import AdditionModP, MultiplicationModP


def testModP():
    for p in range(2, 12):
        add = AdditionModP(p)
        mult = MultiplicationModP(p)

        assert add.isClosed()
        assert add.isSemiGroup()
        assert add.isMonoid(0)
        print(add.isGroup(0, lambda a: (p - a) % p).because)

        def invert(a):
            return next((b for b in range(1, p) if (a * b) % p == 1), None)

        mig = mult.isGroup(1, invert)
        # print(cayleyTable(range(1, p), mult.op))

        print(mig.because)
