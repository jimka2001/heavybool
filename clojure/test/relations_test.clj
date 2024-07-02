(ns relations-test
  (:require [examples.relations :as sut]
            [heavy-bool :refer [+bool +not +tag +heavy-bool]]
            [clojure.test :as t]
            [clojure.test :refer [deftest is testing]]))

(deftest t-equivalence
  (testing "equivalence"
    (is (+bool (sut/is-equivalence (range 1 10) =)))
    (is (+bool (+not (sut/is-equivalence (range 1 10) not=))))
))

(deftest t-partial-order
  (testing "partial order"
    (is (+bool (sut/is-strict-partial-order (range 1 10) <)))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) <=))))
    (is (+bool (+not (sut/is-strict-partial-order (range 1 10) =))))
    ))

(deftest t-antisymmetric
  (testing "antisymmetric"
    (let [population (range 1 1000)
          rel (fn [a b] (+heavy-bool (= 0 (mod a b))))]
      (is (sut/is-antisymmetric population rel)))))
