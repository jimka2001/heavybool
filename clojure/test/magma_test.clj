(ns magma-test
  (:require [util :refer [member]]
            [magma :as sut]
            [mod-p :refer [mod-p]]
            [heavy-bool :refer [+bool +not +and +or +forall +exists +conj +conj-false heavy-bool?]]
            [gaussian-int :refer [gaussian-int-mod-p]]
            [clojure.math :refer [sqrt ceil]]
            [clojure.test :refer [deftest testing is]]))

(deftest t-is-closed
  (testing "is-closed"
    (let [hb (sut/is-closed (range 10)
                            (fn [a b] (+ a b))
                            (fn [a] (+conj-false [(< a 100) ()] {:a a :reason ">= 100"})))]
      (is (heavy-bool? hb))
      (is (+bool hb)))
    (let [hb (sut/is-closed (range 10)
                            (fn [a b] (+ a b))
                            (fn [a] (+conj-false [(< a 10) ()] {:a a :reason ">= 10"})))]
      (is (heavy-bool? hb))
      (is (not (+bool hb))))))



(deftest t-is-associative
  (testing "is-associative"
    (is (not (+bool (sut/is-associative (range 10) (fn [a b] (- a b)) sut/default-equal))))))

(deftest t-is-commutative
  (testing "is-commutative"
    (is (not (+bool (sut/is-commutative (range 10) (fn [a b] (- a b)) sut/default-equal))))))

(deftest t-is-identity
  (testing "is-identity"
    (is (not (+bool (sut/is-identity (range 10) (fn [a b] (- a b)) 0 sut/default-equal))))))


(deftest t-find-identity
  (testing "find-identity"
    (let [[bool reason] (sut/find-identity (range 10) (fn [a b] (+ a b)) sut/default-equal)]
      (is bool)
      (is (= 0 (:witness (first reason)))))))

(deftest t-mod-2
  (testing "find-mod-2"
    (let [mod-2 (mod-p 2)]
      (is (+bool (sut/is-group (:gen mod-2)
                                       (:op mod-2)
                                       (:ident mod-2)
                                       (:invert mod-2)
                                       (:member mod-2)
                                       (:equiv mod-2)))))))

(deftest t-mod-3
  (testing "find-mod-3"
    (let [mod-3 (mod-p 3)]
      (is (+bool (sut/is-group (:gen mod-3)
                                       (:op mod-3)
                                       (:ident mod-3)
                                       (:invert mod-3)
                                       (:member mod-3)
                                       (:equiv mod-3)))))))

(defn prime? [p]
  (or (= 2 p)
      (and (not= 0 (mod p 2))
           (every? (fn [f]
                     (not= (mod p f) 0))
                   (range 3 (inc (ceil (sqrt p))) 2)))))

(deftest t-prime?
  (testing "prime?"
     (is (prime? 2))
     (is (prime? 3))
     (is (not (prime? 4)))
     (is (prime? 5))
     (is (not (prime? 6)))
     (is (prime? 7))))



(deftest t-mod-prime
  (testing "find mod prime"
    (doseq [n (range 2 50)
            :let [mod-n (mod-p n)]]
      (is (= (prime? n)
             (+bool (sut/is-group (:gen mod-n)
                                  (:op mod-n)
                                  (:ident mod-n)
                                  (:invert mod-n)
                                  (:member mod-n)
                                  (:equiv mod-n))))))))


(defn test-gaussian [p]
  (let [m (gaussian-int-mod-p p)
        f (+conj-false (sut/is-field (:gen m)
                                     (:add m)
                                     (:mult m)
                                     (:zero m)
                                     (:one m)
                                     (:add-inv m)
                                     (:mult-inv m)
                                     (:member m)
                                     (:equiv m)) {:reason "not a field"})]
    (+conj f {:p p})))

(deftest t-gaussian
  (testing "gaussian int"
    (doseq [p (range 2 10)
            :let [f (test-gaussian p)]]
      (if (member p [3 7])
        (is (+bool f))
        (is (+bool (+not f)))))))
                                
          
