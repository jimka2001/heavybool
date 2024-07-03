(ns relations-test
  (:require [relations :as sut]
            [util :refer [power-set gcd]]
            [heavy-bool :refer [+bool +not +tag +heavy-bool]]
            [clojure.set :refer [subset?]]
            [clojure.test :refer [deftest is testing]]))

;;(def hb-< (sut/lift-relation <))
(def proper-subset? (fn [a b] (and (not= a b)
                                      (subset? a b))))
(def divides (fn [a b] (= (mod a b) 0)))
(def coprime (fn [a b] (= 1 (gcd a b))))

(deftest t-reflexive
  (testing "reflexive"
    (is (+bool (sut/is-reflexive (range 1 100) =)))
    (is (+bool (sut/is-reflexive (range 1 100) <=)))
    (is (+bool (sut/is-reflexive (range 1 100) >=)))
    (is (+bool (+not (sut/is-reflexive (range 1 100) <))))
    (is (+bool (+not (sut/is-reflexive (range 1 100) >))))
    (is (+bool (sut/is-reflexive (power-set (into #{} (range 10))) subset?)))
    (is (+bool (+not (sut/is-reflexive (power-set (into #{} (range 10))) proper-subset?))))
    (is (+bool (sut/is-reflexive (range 1 100) divides)))))

(deftest t-irreflexive
  (testing "irreflexive"
    ;; is not equal to
    (is (+bool (sut/is-irreflexive (range 100) not=)))
    
    ;; is coprime to on the integers larger than 1
    (is (+bool (sut/is-irreflexive (range 2 100) coprime)))

    ;; is a proper subset of
    (is (+bool (sut/is-irreflexive (power-set (into #{} (range 10))) proper-subset?)))

    ;; is greater than
    (is (+bool (sut/is-irreflexive (range 1 100) >)))

    ;; is less than
    (is (+bool (sut/is-irreflexive (range 1 100) <)))))


(deftest t-asymmetric
  (testing "asymmetric"
    (is (+bool (sut/is-asymmetric (range 1 100) <)))
    (is (+bool (sut/is-asymmetric (range 1 100) >)))))


(deftest t-equivalence
  (testing "equivalence"
    (is (+bool (sut/is-equivalence (range 1 10) =)))
    (is (+bool (+not (sut/is-equivalence (range 1 10) not=))))))

(deftest t-partial-order
  (testing "partial order"
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) <=))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) =))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) >=))))
    (is (+bool (sut/is-partial-order (range 1 10) <=)))
    (is (+bool (sut/is-partial-order (range 1 10) >=)))
    (is (+bool (sut/is-strict-partial-order (range 1 10) <)))
    (is (+bool (sut/is-strict-partial-order (range 1 10) >)))
    ))

(deftest t-antisymmetric
  (testing "antisymmetric"
    (let [population (range 1 1000)]
      (is (sut/is-antisymmetric population divides)))))

(deftest t-symmetric
  (testing "symmetric"
    (is (+bool (sut/is-symmetric (range 100) =)))
    (is (+bool (+not (sut/is-symmetric (range 100) <))))
    (is (+bool (+not (sut/is-symmetric (range 100) >))))
    (is (+bool (+not (sut/is-symmetric (range 100) >=))))
    (is (+bool (+not (sut/is-symmetric (range 100) >=))))
    (is (+bool (+not (sut/is-symmetric (power-set (into #{} (range 10))) proper-subset?))))))


(deftest t-transitive
  (testing "transitive"
    (is (+bool (sut/is-transitive (range 100) =)))
    (is (+bool (sut/is-transitive (range 100) <)))
    (is (+bool (sut/is-transitive (range 100) >)))
    (is (+bool (+not (sut/is-transitive (range 100) not=))))
    (is (+bool (+not (sut/is-transitive (range 100) coprime))))
    (is (+bool (sut/is-transitive (power-set (into #{} (range 6))) proper-subset?)))
    (is (+bool (sut/is-transitive (power-set (into #{} (range 6))) subset?)))
    (is (+bool (sut/is-transitive (range 1 1000) divides)))))

(deftest t-connected
  (testing "connected"
    (is (+bool (sut/is-connected (range 100) <)))
    (is (+bool (sut/is-connected (range 100) >)))
    (is (+bool (sut/is-connected (range 100) <=)))
    (is (+bool (sut/is-connected (range 100) >=)))
    (is (+bool (+not (sut/is-connected (power-set (into #{} (range 8))) proper-subset?))))
    (is (+bool (+not (sut/is-connected (power-set (into #{} (range 8))) subset?))))
    (is (+bool (+not (sut/is-connected (range 1 100) divides))))
    ))
