(ns magma-basic-2-test
  (:require [magma-basic-2 :as sut]
            [util :as ut]
            [mod-p :refer [multiplication-mod-p addition-mod-p]]
            [gaussian-int :refer [gaussian-int-mod-p]]
            [clojure.math :refer [sqrt ceil]]
            [clojure.test :refer [deftest testing is]]))

(deftest t-is-closed
  (testing "is-closed"
    (is (sut/is-closed [true false]
                       (fn [a b] (and a b))
                       (fn [a] (or (= a true)
                                   (= a false)))))

    (is (sut/is-closed (range 10)
                       (fn [a b] (mod (* a b) 10))
                       (fn [a] (and (<= 0 a)
                                    (< a 10)))))

    (is (not (sut/is-closed (range 10)
                       (fn [a b] (+ a b))
                       (fn [a] (< a 10)))))))

(deftest t-is-associative
  (testing "is-associative"
    (is (not (sut/is-associative (range 10) (fn [a b] (- a b)) =)))))

(deftest t-is-commutative
  (testing "is-commutative"
    (is (not (sut/is-commutative (range 10) (fn [a b] (- a b)) =)))))

(deftest t-is-identity
  (testing "is-identity"
    (is (not (sut/is-identity (range 10) (fn [a b] (- a b)) 0 =)))))

(deftest t-has-identity
  (testing "find-identity"
    (is (sut/has-identity (range 10) (fn [a b] (+ a b)) =))))

(deftest t-add-mod-n
  (testing "add mod n"
    (doseq [n (range 2 10)
            :let [gr (addition-mod-p n)]]
      (is (sut/is-group 
           (:gen gr)
           (:op gr)
           (:ident gr)
           (:member gr)
           (:equiv gr))))))

(deftest t-mult-mod-2
  (testing "mult-mod-2"
    (let [mod-2 (multiplication-mod-p 2)]
      (is (sut/is-group (:gen mod-2)
                        (:op mod-2)
                        (:ident mod-2)
                        (:member mod-2)
                        (:equiv mod-2))))))

(deftest t-mult-mod-3
  (testing "find-mult-mod-3"
    (let [mod-3 (multiplication-mod-p 3)]
      (is (sut/is-group (:gen mod-3)
                        (:op mod-3)
                        (:ident mod-3)
                        (:member mod-3)
                        (:equiv mod-3))))))

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


(deftest t-prime-2
  (testing "2nd prime?"
    (let [M 2000]
      (doseq [n (range 2 M)]
        (if (prime? n)
          (is (not (some (fn [f] (= 0 (mod n f)))
                         (range 2 n))))
          (is (some (fn [f] (= 0 (mod n f)))
                    (range 2 n))))))))

(deftest t-mod-prime-3
  (testing "group 3"
    (let [elements (range 0 3)
          ident 1
          op (fn [a b] (mod (* a b) 3))
          member (fn [a] (ut/member a elements))
          ]
      (is (sut/is-monoid elements op ident member =))
      (is (sut/has-inverses elements op ident =))
      (is (sut/is-group elements
                        op
                        ident
                        member
                        =)))))

(deftest t-mod-prime-basic
  (testing "basic find mod prime"
    (doseq [n (range 2 50)
            :let [elements (range 1 n)]]
      (is (= (prime? n)
             (sut/is-group elements
                           (fn [a b] (mod (* a b) n))
                           0
                           (fn [a] (ut/member a elements))
                           =))
          (format "n=%d" n)))))


(deftest t-klein-4
  (testing "Klein 4 group"
    (let [coll [:e :a :b :c]]
      (letfn [(* [x y]
                (cond (= :e x)
                      y

                      (= :e y)
                      x

                      (= x y)
                      :e

                      :else
                      (first (disj #{:a :b :c} x y))))
              (member [x]
                (ut/member x coll))]
        (is (sut/is-group coll
                          *
                          :e
                          member
                          =))))))

