(ns heavy-bool-test
  (:require [heavy-bool :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest t+and
  (testing "and"
    (is (sut/+and (sut/+forall x (range 1 10 3) [(odd? x) '({:forall true})])
                  (sut/+exists x (range 1 10 3) [(odd? x) '({:exists true})])))))

(deftest t+or-1
  (testing "or 1"
    (is
     (sut/+or (sut/+forall x (range 1 10 3) [(odd? x) '({:forall true})])
              (sut/+exists x (range 1 10 3) [(odd? x) '({:exists true})])))))

(deftest t-forall
  (testing "forall"
    (is (= (sut/+forall x [1 2 3] (if (> x 0) [sut/+true
                                        '({:reason "works"})]
                                      [sut/+false '({:reason "fails"})]))
           [true '({:tag x})]))))



(deftest t+or-2
  (testing "or 2"
    (let [m1 '({:reason 1})
          m2 '({:reason 2})]
      (is (= (sut/+or [false m1]
                      [false m2])
             [false m2]))

      (is (= (sut/+or [false m1]
                      [false m1]
                      [false m2])
             [false m2]))

      (is (= (sut/+or [false m1]
                      [false m1]
                      [false m1]
                      [false m2])
             [false m2]))
      (is (= (sut/+or [false m1]
 
                     [false m1]
                      [false m1]
                      [true m2])
             [true m2])))))

(deftest t+and-2
  (testing "and 2"
    (let [m1 '({:reason 1})
          m2 '({:reason 2})]
      (is (= (sut/+and [true m1]
                       [true m2])
             [true m2]))

      (is (= (sut/+and [true m1]
                       [true m1]
                       [true m2])
             [true m2]))

      (is (= (sut/+and [true m1]
                       [true m2]
                       [true m2])
             [true m2]))

      (is (= (sut/+and [true m1]
                       [true m1]
                       [true m2]
                       [true m2])
             [true m2])))))
