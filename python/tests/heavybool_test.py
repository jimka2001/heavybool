import unittest
import os

from src.heavybool import anyM, forallM, existsM, allM, makeHeavyBool, HeavyTrue, HeavyFalse


class HeavyBoolTestCase(unittest.TestCase):

    def test_forallM(self):
        self.assertTrue(forallM(range(10), lambda n: makeHeavyBool(n < 10)))
        self.assertFalse(forallM(range(10), lambda n: makeHeavyBool(n < 5)))
        self.assertTrue(forallM(range(10), lambda n: makeHeavyBool(n < 5)).Not())

    def test_existsM(self):
        self.assertTrue(existsM(range(10), lambda n: n > 5))
        self.assertTrue(existsM(range(10), lambda n: n > 10).Not())

    def test_anyM(self):
        self.assertTrue(anyM(i for i in [HeavyTrue().flag("x"), HeavyFalse().flag("y")]))
        self.assertTrue(anyM(makeHeavyBool(a > 5, {"a": a})
                             for a in range(10)))
        self.assertTrue(anyM(makeHeavyBool(a > b, {"a": a, "b": b})
                             for a in range(10)
                             for b in range(10)))

        self.assertTrue(anyM(makeHeavyBool(a > 10, {"a": a})
                             for a in range(10)).Not())
        self.assertTrue(anyM(makeHeavyBool(a > b, {"a": a, "b": b})
                             for a in range(10)
                             for b in range(a, 10)).Not())

    def test_allM(self):
        self.assertTrue(allM(makeHeavyBool(a < 10, {"a": a})
                             for a in range(10)))
        self.assertTrue(allM(makeHeavyBool(a <= b, {"a": a, "b": b})
                             for a in range(10)
                             for b in range(a, 10)))
        self.assertTrue(allM(makeHeavyBool(a % 2 == 0, {"a": a})
                             for a in range(10)).Not())

    def test_flag(self):
        self.assertTrue(HeavyTrue().flag("x") or HeavyFalse().flag("y"))
        self.assertTrue("x" in (HeavyTrue().flag("x") or HeavyFalse().flag("y")).because[0])

        self.assertTrue(HeavyFalse().flag("x") or HeavyTrue().flag("y"))
        self.assertTrue("y" in (HeavyFalse().flag("x") or HeavyTrue().flag("y")).because[0])

        self.assertFalse(HeavyTrue().flag("x") and HeavyFalse().flag("y"))
        self.assertTrue("y" in (HeavyTrue().flag("x") and HeavyFalse().flag("y")).because[0])

        self.assertFalse(HeavyFalse().flag("x") and HeavyTrue().flag("y"))
        self.assertTrue("x" in (HeavyFalse().flag("x") and HeavyTrue().flag("y")).because[0])
