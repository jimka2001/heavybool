(ns magma-test
  (:require [magma :as sut]
            [util :as ut]
            [mod-p :refer [multiplication-mod-p addition-mod-p]]
            [heavy-bool :refer [+bool +not +exists +tag
                                +annotate +annotate-false heavy-bool? find-witness]]
            [gaussian-int :refer [gaussian-int-mod-p]]
            [clojure.math :refer [sqrt ceil]]
            [clojure.test :refer [deftest testing is]]))

(deftest t-is-closed
  (testing "is-closed"
    (let [hb (sut/is-closed (range 10)
                            (fn [a b] (+ a b))
                            (fn [a] (+annotate-false [(< a 100) ()]
                                                     :a a :reason ">= 100")))]
      (is (heavy-bool? hb))
      (is (+bool hb)))
    (let [hb (sut/is-closed (range 10)
                            (fn [a b] (+ a b))
                            (fn [a] (+annotate-false [(< a 10) ()]
                                                     :a a :reason ">= 10")))]
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
    (let [id (sut/find-identity (range 10) (fn [a b] (+ a b)) sut/default-equal)]
      (is (+bool id))
      (is (= 0 (find-witness id))))))

(deftest t-add-mod-n
  (testing "add mod n"
    (doseq [n (range 2 10)
            :let [gr (addition-mod-p n)]]
      (is (+bool (+annotate (sut/is-group 
                             (:gen gr)
                             (:op gr)
                             (:ident gr)
                             (:invertible gr)
                             (:member gr)
                             (:equiv gr))))))))

(deftest t-mult-mod-2
  (testing "mult-mod-2"
    (let [mod-2 (multiplication-mod-p 2)]
      (is (+bool (+annotate (sut/is-group (:gen mod-2)
                                          (:op mod-2)
                                          (:ident mod-2)
                                          (:invertible mod-2)
                                          (:member mod-2)
                                          (:equiv mod-2))
                            :testing 'mod-p
                            :inv ((:invertible mod-2) 1)
                            :p 2))))))

(deftest t-mult-mod-3
  (testing "find-mult-mod-3"
    (let [mod-3 (multiplication-mod-p 3)]
      (is (+bool (sut/is-group (:gen mod-3)
                                       (:op mod-3)
                                       (:ident mod-3)
                                       (:invertible mod-3)
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

(deftest t-prime-2
  (testing "2nd prime?"
    (let [M 2000]
      (doseq [n (range 2 M)]
        (if (prime? n)
          (is (not (some (fn [f] (= 0 (mod n f)))
                         (range 2 n))))
          (is (some (fn [f] (= 0 (mod n f)))
                    (range 2 n))))))))


(deftest t-mod-prime-basic
  (testing "basic find mod prime"
    (doseq [n (range 2 50)
            :let [elements (range 1 n)
                  ident 0
                  op (fn [a b] (mod (* a b) n))]]
      (is (= (prime? n)
             (sut/is-group elements
                           op
                           ident
                           (fn [a]
                             (+exists [b elements]
                               (and (= (op a b) ident)
                                    (= (op b a) ident))))
                           (fn [a] (ut/member a elements))
                           =))
          (format "n=%d" n)))))

(deftest t-mod-prime
  (testing "find mod prime"
    (doseq [n (range 2 50) 
            :let [mod-n (multiplication-mod-p n)]]
      (is (= (prime? n)
             (+bool (sut/is-group (:gen mod-n)
                                  (:op mod-n)
                                  (:ident mod-n)
                                  (:invertible mod-n)
                                  (:member mod-n)
                                  (:equiv mod-n))))))))

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
              (invertible [x]
                (+tag (+exists [y coll]
                               (= :e (* y x)))
                      :invertible))
              (member [x]
                (+tag (+annotate (ut/member x coll) :x x) :member))
              (equal [x y]
                (+tag (= x y) :equal))]
        (is (+bool (sut/is-group coll
                                 *
                                 :e
                                 invertible
                                 member
                                 equal)))))))

(defn test-gaussian [p]
  (let [m (gaussian-int-mod-p p)
        f (sut/is-field (:gen m)
                        (:add m)
                        (:mult m)
                        (:zero m)
                        (:one m)
                        (:add-inv m)
                        (:mult-inv m)
                        (:member m)
                        (:equiv m))]
    (+annotate f :p p)))

(deftest t-gaussian
  (testing "gaussian int"
    (is (+bool (+not (test-gaussian 2))))
    (doseq [p (range 3 10)
            :let [f (test-gaussian p)]]
      (cond (not (prime? p))
            (is (+bool (+not f)))

            (= 1 (mod p 4)) ;; 4k + 1
            (is (+bool (+not f)))

            :else ;; 4k - 1
            (is (+bool f))))))
