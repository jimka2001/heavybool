(ns magma-basic-test
  (:require [magma-basic :as sut]
            [util :as ut]
            [mod-p :refer [multiplication-mod-p addition-mod-p]]
            [gaussian-int :refer [gaussian-int-mod-p]]
            [clojure.math :refer [sqrt ceil]]
            [clojure.test :refer [deftest testing is]]))

(deftest t-is-closed
  (testing "is-closed"
    (is (sut/is-closed (range 10)
                       (fn [a b] (+ a b))
                       (fn [a] (< a 100))))
    (is (not (sut/is-closed (range 10)
                       (fn [a b] (+ a b))
                       (fn [a] (< a 10)))))))

(deftest t-is-associative
  (testing "is-associative"
    (is (not (sut/is-associative (range 10) (fn [a b] (- a b)) sut/default-equal)))))

(deftest t-is-commutative
  (testing "is-commutative"
    (is (not (sut/is-commutative (range 10) (fn [a b] (- a b)) sut/default-equal)))))

(deftest t-is-identity
  (testing "is-identity"
    (is (not (sut/is-identity (range 10) (fn [a b] (- a b)) 0 sut/default-equal)))))

(deftest t-has-identity
  (testing "find-identity"
    (is (sut/has-identity (range 10) (fn [a b] (+ a b)) sut/default-equal))))

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

(deftest t-mod-prime
  (testing "find mod prime"
    (doseq [n (range 2 50) 
            :let [mod-n (multiplication-mod-p n)]]
      (is (= (prime? n)
             (sut/is-group (:gen mod-n)
                           (:op mod-n)
                           (:ident mod-n)
                           (:member mod-n)
                           (:equiv mod-n)))
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

(defn test-gaussian [p]
  (let [m (gaussian-int-mod-p p)]
    (sut/is-field (:gen m)
                  (:add m)
                  (:mult m)
                  (:zero m)
                  (:one m)
                  (:member m)
                  (:equiv m))))


(deftest t-gaussian
  (testing "gaussian int"
    (is (not (test-gaussian 2)))
    (doseq [p (range 3 10)
            :let [f (test-gaussian p)]]
      (cond (not (prime? p))
            (is (not f))

            (= 1 (mod p 4)) ;; 4k + 1
            (is (not f))

            :else ;; 4k - 1
            (is f)))))
