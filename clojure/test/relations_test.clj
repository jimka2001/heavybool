(ns relations-test
  (:require [relations :as sut]
            [util :refer [power-set]]
            [heavy-bool :refer [+bool +not +tag +heavy-bool]]
            [clojure.test :as t]
            [clojure.set :refer [subset?]]
            [clojure.test :refer [deftest is testing]]))

(def hb-< (sut/lift-relation <))
(def hb-> (sut/lift-relation >))
(def hb-= (sut/lift-relation =))
(def hb-not= (sut/lift-relation not=))
(def hb->= (sut/lift-relation >=))
(def hb-<= (sut/lift-relation <=))
(def hb-subset? (sut/lift-relation subset?))
(def hb-proper-subset? (sut/lift-relation (fn [a b] (and (not= a b)
                                                         (subset? a b)))))
(def hb-divides (fn [a b] (hb-= (mod a b) 0)))

(deftest t-reflexive
  (testing "reflexive"
    (is (+bool (sut/is-reflexive (range 1 100) hb-=)))
    (is (+bool (sut/is-reflexive (range 1 100) hb-<=)))
    (is (+bool (sut/is-reflexive (range 1 100) hb->=)))
    (is (+bool (+not (sut/is-reflexive (range 1 100) hb-<))))
    (is (+bool (+not (sut/is-reflexive (range 1 100) hb->))))
    (is (+bool (sut/is-reflexive (power-set (into #{} (range 10))) hb-subset?)))
    (is (+bool (+not (sut/is-reflexive (power-set (into #{} (range 10))) hb-proper-subset?))))
    (is (+bool (sut/is-reflexive (range 1 100) hb-divides)))))

(deftest t-asymmetric
  (testing "asymmetric"
    (is (+bool (sut/is-asymmetric (range 1 100) hb-<)))
    (is (+bool (sut/is-asymmetric (range 1 100) hb->)))))


(deftest t-equivalence
  (testing "equivalence"
    (is (+bool (sut/is-equivalence (range 1 10) hb-=)))
    (is (+bool (+not (sut/is-equivalence (range 1 10) hb-not=))))))

(deftest t-partial-order
  (testing "partial order"
    (is (+bool (sut/is-strict-partial-order (range 1 10) hb-<)))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) hb-<=))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) hb-=))))
    ))

(deftest t-partial-order
  (testing "partial order"
    (is (+bool (sut/is-strict-partial-order (range 1 10) hb-<)))
    (is (+bool (sut/is-strict-partial-order (range 1 10) hb->)))
    (is (+bool (sut/is-partial-order (range 1 10) hb-<=)))
    (is (+bool (sut/is-partial-order (range 1 10) hb->=)))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) hb-<=))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) hb->=))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) hb-=))))
    ))

(deftest t-antisymmetric
  (testing "antisymmetric"
    (let [population (range 1 1000)
          hb-rel (fn [a b] (hb-= 0 (mod a b)))]
      (is (sut/is-antisymmetric population hb-rel)))))
