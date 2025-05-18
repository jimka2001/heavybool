import unittest

from src.modp import AdditionModP, MultiplicationModP


class ModPTestCase(unittest.TestCase):


    # ELS 2025 DEMO
    def test_ModP(self):
        for p in range(2, 12):
            add = AdditionModP(p)
            mult = MultiplicationModP(p)

            self.assertTrue( add.isClosed())
            self.assertTrue( add.isSemiGroup())
            self.assertTrue(add.isMonoid(0))
            self.assertTrue(add.isGroup(0, lambda a: (p - a) % p))

            def invert(a):
                return next((b for b in range(1, p) if (a * b) % p == 1), None)

            mig = mult.isGroup(1, invert).annotate({"p": p})
            self.assertTrue(mig)

