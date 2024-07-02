(ns relations-test
  (:require [relations :as sut]
            [heavy-bool :refer [+bool +not +tag +heavy-bool]]
            [clojure.test :as t]
            [clojure.test :refer [deftest is testing]]))

(def hb-< (sut/lift-relation <))
(def hb-> (sut/lift-relation >))
(def hb-= (sut/lift-relation =))
(def hb-not= (sut/lift-relation not=))
(def hb->= (sut/lift-relation >=))
(def hb-<= (sut/lift-relation <=))

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
