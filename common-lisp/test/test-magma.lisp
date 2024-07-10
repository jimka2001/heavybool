;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :heavy-bool :package-into :heavy-bool-test)
  (shadow-all-symbols :package-from :heavy-bool-examples :package-into :heavy-bool-test))

(defun test-cayley-tables (n)
  (visit-all-unital-cayley-tables n
                                  (lambda (dyn-op)
                                    (format t "~A~%" (cayley-table (gen-list-finite (1- n)) dyn-op)))))

(define-test test-caley-tables-2
  (test-cayley-tables 2))

(define-test test-caley-tables-3
  (test-cayley-tables 3))

(define-test test-count-groups
  (count-groups 2)
  (count-groups 3))

(define-test test-find-groups
  (find-groups-m 2)
  (find-groups-m 3)
  (find-groups-m 4))

(define-test t-is-closed
  (let ((hb (is-closed (make-instance 'dyn-magma 
                                      :gen (lambda () (range 10))
                                      :op (lambda (a b) (+ a b))
                                      :is-member (lambda (a) (+annotate-false (heavy-bool (< a 100))
                                                                              :a a :reason ">= 100"))))))
    (assert-true (heavy-bool? hb))
    (assert-true (bool hb)))
  (let ((hb (is-closed (make-instance 'dyn-magma
                                      :gen (lambda () (range 10))
                                      :op (lambda (a b) (+ a b))
                                      :is-member (lambda (a) (+annotate-false (heavy-bool (< a 10))
                                                                              :a a :reason ">= 10"))))))
    (assert-true (heavy-bool? hb))
    (assert-false (bool hb))))


(define-test t-is-associative
  (assert-false (bool (is-associative (dyn-magma
                                       :gen (lambda () (range 10))
                                       :op (lambda (a b) (- a b)))))))
                                       

(define-test t-is-commutative
  (assert-false (bool (is-commutative (dyn-magma :gen (lambda () (range 10))
                                                 :op (lambda (a b) (- a b)))))))

(define-test t-is-identity
  (assert-false (bool (is-identity (dyn-magma :gen (lambda () (range 10))
                                              :op (lambda (a b) (- a b)))
                                   0))))


(define-test t-find-identity
  (let ((id (find-identity (dyn-magma :gen (lambda () (range 10))
                                      :op (lambda (a b) (+ a b))))))
    (assert-true (bool id))
    (assert-true (= 0 (find-witness id)))))

(define-test t-add-mod-n
  (loop :for n :in (range 2 10)
        :with m = (make-instance 'addition-mod-p :p n)
        :do (assert-true (bool (is-group m 0 (lambda (a)
                                               (values (mod (- a ) n) t))
                                         )))))

(define-test t-mult-mod-2
  (let ((mod-2 (make-instance 'multiplication-mod-p :p 2)))
    (assert-true (bool (heavy-bool (is-group mod-2 
                                             1
                                             (lambda (a)
                                               (funcall (default-inverter mod-2) a)))

                                   :testing 'mod-p
                                   :p 2)))))

(define-test t-mult-mod-3
  (let ((mod-3 (make-instance 'multiplication-mod-p :p 3)))
    (assert-true (bool (is-group mod3
                                 1
                                 (lambda (a)
                                   (funcall (default-inverter mod-32) a)))))))

(defun prime? (p)
  (or (= 2 p)
      (and (/= 0 (mod p 2))
           (every (lambda (f)
                    (/= (mod p f) 0))
                   (range 3 (1+ (ceiling (sqrt p))) 2)))))

(define-test t-prime?
  (assert-true (prime? 2))
  (assert-true (prime? 3))
  (assert-false (prime? 4))
  (assert-true (prime? 5))
  (assert-false (prime? 6))
  (assert-true (prime? 7)))

(define-test t-mod-prime
  (loop :for n :from 2 to 50
        :with mod-n = (make-instance 'multiplication-mod-p :p n)
        :do (assert-true (= (prime? n)
                            (bool (is-group mod-n
                                            1 
                                            (lambda (a)
                                              (funcall (default-inverter mod-n) a))))))))

(define-test t-klein-4
  (let ((coll '(:e :a :b :c)))
    (labels ((* (x y)
               (cond ((eql :e x)
                      y)
                     ((eql :e y)
                      x)
                     ((eql x y)
                      :e)
                     (t
                      (car (member-if (lambda (u)
                                        (and (not (eql u x))
                                             (not (eql u y))))
                                      '(:a :b :c}))))))
              (invertible (x)
                (+tag (+exists (y coll)
                        (= :e (* y x)))
                      :invertible))
              (mem (x)
                (+tag (+annotate (member x coll) :x x) :member))
              (equal [x y]
                (+tag (= x y) :equal)))
      (assert-true (bool (is-group (dyn-magma :gen (lambda () coll)
                                              :op #'*)
                                   :e
                                   #'invertible))))))


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

(define-test t-gaussian
  (testing "gaussian int"
    (is (+bool (+not (test-gaussian 2))))
    (doseq [p (iota 3 10)
            :let [f (test-gaussian p)]]
      (cond (not (prime? p))
            (is (+bool (+not f)))

            (= 1 (mod p 4)) ;; 4k + 1
            (is (+bool (+not f)))

            :else ;; 4k - 1
            (is (+bool f))))))
