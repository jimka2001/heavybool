;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-examples)

(defclass magma () ())
(defgeneric gen (magma))
(defgeneric op (magma a b))
(defgeneric is-equiv (magma a b))
(defmethod is-equiv ((magma magma) a b)
  (+tag (heavy-bool (equal a b)
                    :a a
                    :b b)
        :is-equiv))

(defgeneric is-member (magma a))
(defgeneric is-closed (magma))

(defmethod is-closed ((magma magma))
  (+tag (+forall (a (gen magma)
                    b (gen magma))
          (is-member magma (op magma a b)))
        :closed))

(defgeneric is-associative (magma))
(defmethod is-associative ((magma magma))
  (+tag (+forall (a (gen magma)
                    b (gen magma)
                    c (gen magma))
          (is-equiv magma (op magma (op magma a b) c)
                    (op magma a (op magma b c))))
        :associative))

(defgeneric is-commutative (magma))
(defmethod is-commutative ((magma magma))
  (+tag (+forall (a (gen magma)
                    b (gen magma))
          (is-equiv magma
                    (op magma a b)
                    (op magma b a)))
        :commutative))

(defgeneric is-identity (magma z))
(defmethod is-identity ((magma magma) z)
  (+tag (+forall (a (gen magma))
          (+annotate (+and (is-equiv magma (op magma a z) a)
                           (is-equiv magma (op magma z a) a))
           :z z))
        :identity))

(defgeneric find-identity (magma))
(defmethod find-identity ((magma magma))
  (+tag (+exists (e (gen magma))
          (is-identity magma e))
        :identity))

;; find inverse knowing the identity
(defgeneric find-inverse-from-ident (magma a z))
(defmethod find-inverse-from-ident ((magma magma) a z)
  (+tag (+exists (b (gen magma))
          (+and (is-equiv magma z (op magma b a))
                (is-equiv magma z (op magma a b))))
        :inverse))

;; find inverse without knowing the identity
(defgeneric find-inverse-wo-ident (magma a))
(defmethod find-inverse-wo-identity ((magma magma) a)
  (let ((e (find-identity magma)))
    (+tag (+and e
                (find-inverse-from-ident magma a e))
          :inverse)))


;; determine whether the given heavy-bool valued function
;; is an inverter for the magma, i.e. is it true that
;; the function, `invert`, is able to find an inverse
;; for every element of the magma.
(defgeneric is-inverter (magma z invert))
(defmethod is-inverter ((magma magma) z invert)
  (declare (type (function (t) heavy-bool) invert))
  (+tag
   (+forall (a (gen magma))
     (let* ((inv (funcall invert a))
            (w (find-witness inv)))
       (+and inv
             (is-member magma w)
             (is-equiv magma z (op magma a w))
             (is-equiv magma z (op magma w a)))))
   :inverter))

(defgeneric is-magma (magma))
(defmethod is-magma ((magma magma))
  (+tag (is-closed magma)
        :magma))

(defgeneric is-semigroup (magma))
(defmethod is-semigroup ((magma magma))
  (+tag (+and (is-magma magma)
              (is-associative magma))
        :semigroup))

(defgeneric is-monoid (magma z))
(defmethod is-monoid ((magma magma) z)
  (+tag (+and (is-semigroup magma)
              (is-identity magma z))
        :monoid))

(defgeneric is-group (magma z invert))
(defmethod is-group ((magma magma) z invert)
  (declare (type (function (t) heavy-bool)
                 invert))
  (+tag (+and (is-monoid magma z)
              (is-inverter magma z invert))
        :group))

(defun gen-list-finite (n)
  (range 0 (1+ n) 1))

(defun mkstring (data &key (delimeter "") (prefix "") (suffix "") (key #'identity))
  (concatenate 'string
               prefix
               (if data
                   (with-output-to-string (str)
                     (princ (funcall key (car data)) str)
                     (loop :for x :in (cdr data)
                           :do (princ delimeter str)
                           :do (princ (funcall key x) str)))
                   "")
               suffix))


(defun cayley-table (elements op)
  (let ((header (mkstring elements
                          :prefix "*|"
                          :delimeter " "))
        (divider (mkstring elements
                           :prefix "-+"
                           :delimeter " "
                           :key (constantly "-"))))
    (with-output-to-string (str)
      (format str "~%")
      (princ header str)
      (format str "~%")
      (princ divider str)
      (format str "~%")
      (princ 
       (mkstring (mapcar (lambda (x)
                         (mkstring (mapcar (lambda (y)
                                           (funcall op x y))
                                         elements)
                                   :prefix (format nil "~A|" x)
                                   :delimeter " ")) 
            
                       elements)
                 :delimeter (format nil "~%"))
       str))))

(defun random-cayley-table (n)
  (let ((tbl (make-hash-table)))
    (loop :for row :from 1 :below n
          :do (loop :for col :from 1 :below n
                    :do (setf (gethash (list row col) tbl)
                              (random n))))
    tbl))

(defun is-ring (gen is-member + * - one zero)
  (labels ((is-local-member (x)
             (heavy-bool (funcall is-member x))))
  
    (let ((ma (make-instance 'dyn-magma :gen gen :op + :is-member #'is-local-member))
          (mb (make-instance 'dyn-magma :gen gen :op * :is-member #'is-local-member)))
      (+tag (+and (is-group ma zero -)
                  (is-monoid mb one)
                  (+forall (a (funcall gen)
                              b (funcall gen)
                              c (funcall gen))
                    (+and (+tag (is-equiv ma
                                          (funcall * a (funcall + b c))
                                          (funcall + (funcall * a b) (funcall * a c)))
                                :left-distributive)
                          (+tag (is-equiv ma
                                          (funcall * (funcall + b c) a)
                                          (funcall + (funcall * b a) (funcall * c a)))
                                :right-distributive))))
            :ring))))

(defun is-field (gen is-member + * - 1/ zero one)
  (labels ((is-local-member (x)
             (heavy-bool (funcall is-member x)))
           (non-zero-gen ()
             (remove-if (lambda (x) (equal zero x))
                        (funcall gen))))
    (let ((ma (make-instance 'dyn-magma :gen gen :op + :is-member #'is-local-member))
          (mb (make-instance 'dyn-magma :gen gen :op * :is-member #'is-local-member))
          (mz (make-instance 'dyn-magma :gen #'non-zero-gen
                                        :op *
                                        :is-member #'is-local-member)))
      (+tag (+and (+not (is-equiv ma one zero))
                  (is-commutative mb)
                  (is-ring gen is-member
                           + * -
                           one zero)
                  (is-inverter mz one 1/))
            :field))))


(defun visit-all-unital-cayley-tables (n visitor)
  (loop :for i :from (1- (expt n (* (1- n) (1- n)))) :downto 1
        :do (funcall visitor (lambda (a b)
                               (cond ((= a 0)
                                      b)
                                     ((= b 0)
                                      a)
                                     (t
                                      (let ((pos (+ (* (1- a) (1- n))
                                                    (1- b))))
                                        (mod (/ i (expt n pos)) n))))))))

(defun visit-all-cayley-tables (n visitor)
  (loop :for i :from (1- (expt n (* n n))) :downto 1
        :do (funcall visitor (lambda (a b)
                               (let ((pos (+ (* a n) b)))
                                 (mod (/ i (expt n pos)) n))))))


(defun count-groups (n)
  (let ((elements (gen-list-finite (1- n)))
        (groups 0)
        (abelain-groups 0)
        (monoids 0)
        (commutative-monoids 0)
        (commutative-semigroups 0)
        (semigroups 0)
        (tries 0)
        (commutatives 0))
    (visit-all-cayley-tables n
                             (lambda (dyn-op)
                               (let* ((dm (make-instance 'dyn-magma
                                                        :gen (constantly elements)
                                                        :op dyn-op
                                                        :is-member (lambda (a) (and (member a elements) t))))
                                      (ab (is-commutative dm)))
                                 (incf tries)
                                 (+if ab
                                      (incf commutatives))
                                 (+or (let ((tf (is-group dm 0 (lambda (x) (find-inverse-from-ident dm x 0)))))
                                        (+if (+not tf)
                                             tf
                                             (progn (incf groups)
                                                    (incf monoids)
                                                    (incf semigroups)
                                                    (+if ab
                                                         (progn (incf abelain-groups)
                                                                (incf commutative-monoids)
                                                                (incf commutative-semigroups)))
                                                    (format t "found")
                                                    (+if ab (format t " Abelian"))
                                                    (format t " group ")
                                                    (format t "~A" (cayley-table elements dyn-op))
                                                    (+annotate tf
                                                               :reason "found a group"
                                                               :cayley-table (cayley-table elements dyn-op)))))
                                      (let ((tf (is-monoid dm 0)))
                                        (+if (+not tf)
                                             tf
                                             (progn (incf monoids)
                                                    (incf semigroups)
                                                    (+if ab
                                                         (progn (incf commutative-monoids)
                                                                (incf commutative-semigroups)))
                                                    (+annotate tf
                                                               :reason "found a monoid"
                                                               :cayley-table (cayley-table elements dyn-op)))))
                                      (let ((tf (is-semigroup dm)))
                                        (+if (+not tf)
                                             tf
                                             (progn (incf semigroups)
                                                    (+if ab
                                                         (incf commutative-semigroups))
                                                    (+annotate tf
                                                               :reason "found a semigroup"
                                                               :caley-table (cayley-table elements dyn-op)))))))))
    (format t "magmas:     ~A commutative ~A~%" tries commutatives)
    (format t "semigroups: ~A commutative ~A~%" semigroups commutative-semigroups)
    (format t "monoids:    ~A commutative ~A~%" monoids commutative-monoids)
    (format t "groups:     ~A commutative ~A~%" groups abelain-groups)))

(defun find-groups-m (n)
  (let ((elements (gen-list-finite (1- n)))
        (groups 0)
        (tries 0))
    (visit-all-unital-cayley-tables
     n (lambda (add)
         (incf tries)
         (let* ((dm (make-instance 'dyn-magma
                                  :is-member (lambda (a)
                                               (and (member a elements) t))
                                  :op add
                                  :gen (constantly elements)))
                (ig (is-group dm 0 (lambda (x)
                                     (find-inverse-from-ident dm x 0)))))
           (+if ig
                (let ((table (cayley-table elements add)))
                  (incf groups)
                  (format t "found a group ~A: ~A%" table ig))))))
    (format t "groups: ~A/~A~%" groups tries)))
