(ns heavy-bool-test
  (:require [heavy-bool :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest t+and
  (testing "and"
    (is (sut/+and (sut/+forall [x (range 1 10 3)] [(odd? x) '({:forall true})])
                  (sut/+exists [x (range 1 10 3)] [(odd? x) '({:exists true})])))))

(deftest t+or-1
  (testing "or 1"
    (is
     (sut/+or (sut/+forall [x (range 1 10 3)] [(odd? x) '({:forall true})])
              (sut/+exists [x (range 1 10 3)] [(odd? x) '({:exists true})])))))

(deftest t-forall
  (testing "forall"
    (is (= (sut/+forall [x [1 2 3]] (if (> x 0) [sut/+true
                                        '({:reason "works"})]
                                      [sut/+false '({:reason "fails"})]))
           [true '({:var x})]))))

(deftest t-forall-syntax
  (testing "forall syntax"
    (is (sut/+bool (sut/+forall [a (range 10)
                                 b (range 10)
                                 :when (> a 5)
                                 :when (> b a)]
                     (sut/+heavy-bool (> b 5)))))
    (is (sut/+bool (sut/+forall [a (range 10)
                                 b (range 10)
                                 :let [c (+ a b)]]
                     (sut/+heavy-bool (>= c a)))))
    (is (sut/+bool (sut/+forall [a (range 10)
                                 :when (> a 2)
                                 b (range 10)
                                 :let [c (+ a b)]]
                     (sut/+heavy-bool (>= c a)))))
    ))

(deftest t-exists-syntax
  (testing "exists syntax"
    (is (sut/+bool (sut/+exists [a (range 10)
                                 b (range 10)
                                 :when (> a 5)
                                 :when (> b a)]
                     (sut/+heavy-bool (> b 5)))))
    (is (sut/+bool (sut/+exists [a (range 10)
                                 b (range 10)
                                 :let [c (+ a b)]]
                     (sut/+heavy-bool (>= c a)))))
    (is (sut/+bool (sut/+exists [a (range 10)
                                 :when (> a 2)
                                 b (range 10)
                                 :let [c (+ a b)]]
                     (sut/+heavy-bool (>= c a)))))
))
    

(deftest t-forall-examples
  (sut/+forall [a (range 10)
                b (range 10)
                :when (> a 5)
                :when (> b a)]
    (sut/+heavy-bool (> b 5)))

  (sut/+forall [a (range 10)
                b (range 10)
                :when (> a 5)
                :when (> b a)]
    (sut/+heavy-bool (> b 5))))

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
