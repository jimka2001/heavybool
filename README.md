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

The Clojure library is called [heavy-bool](clojure/README.md)

## Common Lisp

## Python
