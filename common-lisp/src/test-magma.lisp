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
        :do (let ((n n)
                  (m (make-instance 'addition-mod-p :p n)))
              (assert-true (bool (is-group m 0 (lambda (a)
                                                 (heavy-bool t :witness (mod (- a) n)))))))))

(define-test t-mult-mod-2
  (let ((mod-2 (make-instance 'multiplication-mod-p :p 2)))
    (assert-true (bool (heavy-bool (is-group mod-2 
                                             1
                                             (lambda (a)
                                               (find-inverse-from-ident mod-2 a 1)))

                                   :testing 'mod-p
                                   :p 2)))))

(define-test t-mult-mod-3
  (let ((mod-3 (make-instance 'multiplication-mod-p :p 3)))
    (assert-true (bool (is-group mod-3
                                 1
                                 (lambda (a)
                                   (find-inverse-from-ident mod-3 a 1)))))))

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

(let* ((n 4)
       (mod-n (make-instance 'multiplication-mod-p :p n))
       (g (is-group mod-n
                    1 
                    (lambda (a)
                      (find-inverse-from-ident mod-n a 1)))))
  (list (prime? n)
        g
        (bool g)
        (eql (prime? n)
             (bool g))

        ))


(define-test t-mod-prime-4
  (let* ((n 4)
         (mod-n (make-instance 'multiplication-mod-p :p n))
         (g (is-group mod-n
                      1 
                      (lambda (a)
                        (find-inverse-from-ident mod-n a 1)))))
    (assert-false (bool g)
                  :tag :mod-prime-4)))

(define-test t-mod-prime
  (loop :for n :from 2 to 4
        :do (let* ((n n)
                   (mod-n (make-instance 'multiplication-mod-p :p n))
                   (g (is-group mod-n
                                1 
                                (lambda (a)
                                  (find-inverse-from-ident mod-n a 1))))
                   
                  )
              (assert-true (eql (prime? n)
                                (bool g))
                           :tag :mod-prime
                           :message (format nil "n=~A, prime=~A g=~A" n (prime? n) g)))))

(define-test t-klein-4
  (let ((coll '(:e :a :b :c)))
    (labels ((times (x y)
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
                                      '(:a :b :c))))))
             (invertible (x)
               (+exists (y coll)
                 (and (eql :e (times y x))
                      (eql :e (times x y))))))
      (assert-true (eql :e (times :a :a)))
      (assert-true (eql :c (times :c :e)))
      (assert-true (eql :e (times :e :e)))
      (assert-true (bool (is-group (dyn-magma :gen (lambda () coll)
                                              :op #'times)
                                   :e
                                   #'invertible))))))

(define-test t-not-klein-4
  (let ((coll '(:e :a :b :c)))
    (labels ((times (x y)
               (cond ((eql :e x)
                      y)
                     ((eql :e y)
                      x)
                     ((eql x y)
                      x ;; :e not a group because this case should return :e
                      )
                     (t
                      (car (member-if (lambda (u)
                                        (and (not (eql u x))
                                             (not (eql u y))))
                                      '(:a :b :c))))))
             (invertible (x)
               (+exists (y coll)
                 (and (eql :e (times y x))
                      (eql :e (times x y))))))
      (assert-false (bool (is-group (dyn-magma :gen (lambda () coll)
                                               :op #'times)
                                    :e
                                    #'invertible))))))


(defun test-gaussian (p)
  (let* ((m (gaussian-int-mod-p p))
         (f (is-field (slot-value m 'gen)
                      (slot-value m 'member)
                      (slot-value m 'add)
                      (slot-value m 'mult)
                      (slot-value m 'add-inv)
                      (slot-value m 'mult-inv)
                      (slot-value m 'zero)
                      (slot-value m 'one))))
    (heavy-bool f :p p)))

(define-test t-gaussian
  (assert-true (bool (+not (test-gaussian 2))))
  (loop :for p :in (range 3 13)
        :do (let* ((p p)
                  (f (test-gaussian p)))
              (cond ((not (prime? p))
                     (assert-true (bool (+not f))
                                  :message (format nil "p=~A" p)))
                    ((= 1 (mod p 4)) ;; 4k + 1
                     (assert-true (bool (+not f))
                                  :message (format nil "p=~A" p)))
                    (t ;; 4k - 1
                     (assert-true (bool f)
                                  :message (format nil "p=~A" p)))))))

