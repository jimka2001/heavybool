# HeavyBool

Implementation of Heavy-Boolean in several programming languages


A Heavy-Boolean is an object which should be treated as a true or
false in a Boolean context, but has some meta-data attached to it.
The motivating example is to implement universal and existential
quantifiers.

For example:
There exists x in A such that
   there exists y in A such that
      for every z in A, p(x,y,z) holds

If this statement is false, then there is some triple (x,y,z) which
serves as a counter example.   However, in many programming languages
(all that I know of), whenever this expression is found to be false, 
then x, y, and z are already out of scope and they cannot be encorporated
into an error message.

This implementation of Heavy-Boolean provides universal and existential
quantifiers which leave a trail in meta data allowing the programmer to
programmically understand why existential quantifiers are true and why
universal quantifiers are false.

## Scala

## Clojure

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
           "WHAT TEXT TO PUT HERE?"))))
```

By pulling the `is` outside the loop, the test will fails early, as
soon as it discovers one failure.  However, there is no way to
construct the second argument of `is` which should indicate the values
of `p1`, `p2`, and `p3` which constitute the counterexample.


## Common Lisp

## Python
