# Clojure implementation of [HeavyBool](../README.md)


## Motivation

Here is an example.  Consider the following test case which uses the `clojure.test` API.

```
(deftest t-plus-associative
  (binding [*time-out* polynomial-time-out]
    (testing-with-timeout "plus associativity"
      ;; check associativity
      (doseq [p1 polynomials
              p2 polynomials
              p3 polynomials]
        (is (sut/poly-almost-equal 0.001
                                   (sut/poly-plus (sut/poly-plus p1 p2) p3)
                                   (sut/poly-plus p1 (sut/poly-plus p2 p3)))
            (format "Discovered non-associative input for poly-plus\np1=%s\np2=%sp3=%s"
                    p1 p2 p3))))))
```

The test tries to find an example (counter example) where the function `poly-plus` fails
to be associative.  If such is found, the call to `is` registers the counter example,
but the `doseq` loop continues.  Thus, if N is the length of `polynomials`, then worst case
the loop will report N-cubed many violations.   

If we wish the test to simply fail on the first violation, we might be
tempted to rewrite the code as follows.

```
(deftest t-plus-associative-b
  (binding [*time-out* polynomial-time-out]
    (testing-with-timeout "plus associativity"
      ;; check associativity
       (is (every? (fn [p1]
                     (every? (fn [p2]
                               (every? (fn [p3]
                                         (sut/poly-almost-equal 0.001
                                                                (sut/poly-plus 
                                                                  (sut/poly-plus p1 p2)
                                                                  p3)
                                                                (sut/poly-plus
                                                                  p1
                                                                  (sut/poly-plus p2 p3))))
                                       polynomials)) 
                             polynomials)) 
                   polynomials)
           ;; regretably, p1, p2, and p3 are out of scope
           "WHAT TEXT TO PUT HERE?"))))
```

By pulling the `is` outside the loop, the test will fails early, as
soon as it discovers one failure.  However, there is no way to
construct the second argument of `is` which should indicate the values
of `p1`, `p2`, and `p3` which constitute the counterexample.

