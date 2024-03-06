(ns relations-test
  (:require [relations :as sut]
            [heavy-bool :refer [+bool +not]]
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
