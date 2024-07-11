;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-examples)


(defun is-reflexive (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (+tag
   (+forall (x (funcall gen))
     (funcall (+lift <) x x))
   :reflexive))

(defun is-irreflexive (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (+tag (+not (+exists (x (funcall gen))
                (funcall (+lift <) x x)))
        :irreflexive))


(defun is-symmetric (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))
    ;; a < b ==> b < a
    (+tag
     (+forall (x (funcall gen)
                 y (funcall gen))
       (+implies (rel x y)
                 (rel y x)))
     :symmetric)))

(defun is-asymmetric (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))
    (+tag
     (+forall (x (funcall gen)
                 y (funcall gen))
       (+implies (rel x y)
                 (+not (rel y x))))
     :assymmetric)))

(defun is-antisymmetric (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))
    (+tag
     (+forall (x (funcall gen)
                 y (funcall gen))
       (+implies (+and (rel x y)
                       (rel y x))
                 (heavy-bool (equal x y))))
     :antisymmetric)))

(defun is-connected (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))
    (+tag
     (+forall (a (funcall gen)
                 b (funcall gen))
       (+implies (heavy-bool (not (equal a b)))
                 (+or (rel a b)
                      (rel b a))))
     :connected)))

(defun is-strongly-connected (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))
    (+tag
     (+forall (a (funcall gen)
                 b (funcall gen))
         (+or (rel a b)
              (rel b a)))
     :strongly-connected)))


(defun is-transitive (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (flet ((rel (a b)
           (heavy-bool (funcall < a b)
                       :a a
                       :b b)))

    (+tag
     (+forall (x (funcall gen)
                 y (funcall gen))
       (+implies (rel x y)
                 (+forall (z (funcall gen))
                   (+implies (rel y z)
                             (rel x z)))))
     :transitive)))

(defun is-equivalence (gen =)
  (declare (type (function () t) gen)
           (type (function (t t) t) =))
  (+tag
   (+and (is-reflexive gen =)
         (is-symmetric gen =)
         (is-transitive gen =))
   :equivalence))

(defun is-partial-order  (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (+tag
   (+and (is-reflexive gen <)
         (is-antisymmetric gen <)
         (is-transitive gen <))
   :partial-order))

(defun is-strict-partial-order (gen <)
  (declare (type (function () t) gen)
           (type (function (t t) t) <))
  (+tag
   (+and (is-irreflexive gen <)
                   (is-transitive gen <)
                   (is-asymmetric gen <))
   :strict-partial-order))

