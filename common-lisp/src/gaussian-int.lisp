;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-examples)

(defun gaussian? (g)
  "Bolean predicate detecting whether the given value is a 2 element vector of integers"
  (and (listp g)
       (= 2 (length g))
       (integerp (car g))
       (integerp (cadr g))))

(defclass gaussian-int-mod-p ()
  ((p :initarg :p)
   (gen :initarg :gen)
   (op :initarg :op)
   (equiv :initarg     :equiv)
   (mult :initarg     :mult)
   (one :initarg     :one)
   (zero :initarg     :zero)
   (add :initarg     :add)
   (add-inv :initarg     :add-inv)
   (mult-inv :initarg     :mult-inv)
   (member :initarg     :member)))


(defun gaussian-int-mod-p (p)
  "factory function for class of same name
  These slots are the data necessary to make a call to the `is-field` function.
  The values of each key is determined in a way so that if p is an odd prime
  of the form 4k-1, then the set of Gaussian integers modulo p is a field.
  Otherwise the set is not a field.
  "
  (let ((zero '(0 0))
        (one '(1 0)))
    (labels ((gmod1 (a)
               (mod a p))
             (gmod2 (a b)
               (list (gmod a) (gmod b)))
             (gmod (a &optional b)
               (if (null b)
                   (gmod1 a)
                   (gmod2 a b)))
             (add (a b)
               (assert (gaussian? a))
               (assert (gaussian? b))
               (destructuring-bind (x y) a
                 (destructuring-bind (u v) b
                   (gmod (+ x u) (+ y v)))))
             (subtract (a b)
               (assert (gaussian? a))
               (assert (gaussian? b))
               (destructuring-bind (x y) a
                 (destructuring-bind (u v) b
                   (gmod (- x u) (- y v)))))
             (add-inv (a)
               (assert (gaussian? a))
               (heavy-bool t :witness (subtract zero a)
                             :zero zero
                             :a a))
             (mult (a b)
               (assert (gaussian? a))
               (assert (gaussian? b))
               (destructuring-bind (x y) a
                 (destructuring-bind (u v) b
                   (gmod (- (* u x) (* v y))
                         (+ (* u y) (* v x))))))
             (mult-inv (ab)
               (assert (gaussian? ab))
               (destructuring-bind (a b) ab
                 (let ((denom (gmod (+ (* a a)
                                       (* b b)))))
                   (if (= 0 denom)
                       (heavy-bool nil
                                   :reason "gaussian integer ab not invertible mod p"
                                   :p p
                                   :ab ab)
                       (let ((found (member-if (lambda (z) (= 1 (gmod (* z denom))))
                                               (range 1 (1+ p)))))
                         (if found
                             (heavy-bool t
                                         :witness (gmod (* a (car found)) (- (* b (car found))))
                                         :z (car found))
                             (heavy-bool nil
                                         :reason "gaussian integer ab not invertible mod p"
                                         :p p
                                         :ab ab)))))))
             (op (a b)
               (assert (gaussian? a))
               (assert (gaussian? b))
               (add a b))
             (gen ()
               ;; return lazy list of gaussian numbers
               (loop :for u :in (range 0 p)
                     :nconc (loop :for v :in (range 0 p)
                                  :collect (list u v))))
             (equiv (a b)
               (assert (gaussian? a))
               (assert (gaussian? b))
               (+tag (heavy-bool (equal a b)
                                 :a a
                                 :b b)
                     :equal))
             (mem (a)
               (assert (gaussian? a))
               (destructuring-bind (x y) a
                 (+tag
                  (heavy-bool
                   (cond ((< x 0)
                          (heavy-bool nil :reason "x < 0"))
                         ((< y 0)
                          (heavy-bool nil :reason "y < 0"))
                         ((>= x p)
                          (heavy-bool nil :reason "x >= p"))
                         ((>= y p)
                          (heavy-bool nil :reason "y >= p"))
                         (t t))
                   :p p :x x :y y)
                  :member))))
      (make-instance 'gaussian-int-mod-p
       :p p
       :gen #'gen
       :op #'op
       :equiv #'equiv
       :mult #'mult
       :one one
       :zero zero
       :add #'add
       :add-inv #'add-inv
       :mult-inv #'mult-inv
       :member #'mem))))
