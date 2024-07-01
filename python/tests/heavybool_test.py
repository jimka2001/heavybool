from src.heavybool import anyM, forallM, existsM, allM, makeHeavyBool


def testForallM():
    assert forallM(range(10), lambda n: n < 10)
    assert forallM(range(10), lambda n: n < 5).Not()


def testExistsM():
    assert existsM(range(10), lambda n: n > 5)
    assert existsM(range(10), lambda n: n > 10).Not()

def testAnyM():
    assert anyM(i for i in [HeavyTrue().flag("x"), HeavyFalse().flag("y")])
    assert anyM(makeHeavyBool(a > 5, {"a", a})
                for a in range(10))
    assert anyM(makeHeavyBool(a > b, {"a": a, "b": b})
                for a in range(10)
                for b in range(10))
    
    assert anyM(makeHeavyBool(a > 10, {"a", a})
                for a in range(10)).Not()
    assert anyM(makeHeavyBool(a > b, {"a": a, "b": b})
                for a in range(10)
                for b in range(a, 10)).Not()

def testAllM():
    assert allM(makeHeavyBool(a < 10 5, {"a", a})
                for a in range(10))
    assert allM(makeHeavyBool(a <= b, {"a": a, "b": b})
                for a in range(10)
                for b in range(a, 10))
    assert allM(makeHeavyBool(a % 2 == 0, {"a": a})
                for a in range(10)).Not()




def testFlag():
    assert HeavyTrue().flag("x") or HeavyFalse().flag("y")
    assert HeavyFalse().flag("x") or HeavyTrue().flag("y")
    assert HeavyTrue().flag("x") and HeavyFalse().flag("y")
    assert HeavyFalse().flag("x") and HeavyTrue().flag("y")
