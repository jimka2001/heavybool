;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool)

(defun range (start &optional stop step)
  (cond ((null stop)
         (range 0 start step))
        ((null step)
         (range start stop 1))
        (t
         (loop :for i :from start :below stop :by step
              :collect i))))
        
(defclass heavy-bool ()
  ((bool :initarg :bool :reader bool)
   (reason :type list :initform nil :initarg :reason :reader reason
           :documentation "a possibly empty list of non-empty plists")))

(defgeneric heavy-bool? (hb))
(defmethod heavy-bool? ((hb heavy-bool))
  t)
(defmethod heavy-bool? ((otherwise t))
  nil)

(defmethod plist? (plist)
  (or (null plist)
      (and (symbolp (car plist))
           (not (null (cdr plist)))
           (plist? (cddr plist)))))
(defmethod initialize-instance :after ((hb heavy-bool) &key &allow-other-keys)
  (assert (listp (reason hb)) (hb) "expecting a list, got ~A" (reason hb))
  (assert (not (eql (class-of hb) (find-class 'heavy-bool)))
          (hb) "can only create instance of heavy-true or heavy-false")
  (loop :for plist :in (reason hb)
        :do (assert plist (plist)
                    "expecting a list of non-empty plists, found ~A in list ~A" plist (reason hb))
        :do (assert (plist? plist) (hb plist)
                    "expecting a list of plists, found ~A in list ~A" plist (reason hb))))

(defclass heavy-true (heavy-bool)
  ((bool :initform t)))

(defclass heavy-false (heavy-bool)
  ((bool :initform nil)))

(defmethod print-object ((hb heavy-bool) stream)
  (print-unreadable-object (hb stream :type t :identity nil)
    (if (bool hb)
        (progn (format stream "T")
               (if (not (eq (bool hb) t))
                   ;; if there is a true value other than t, then print it
                   (format stream " [~A]" (bool hb))))
        (format stream "F"))
    (when (reason hb)
      (format stream " ~S" (reason hb)))))

(defun +lift (pred)
  (declare (type (function (t &rest t) t) pred))
  "+lifts takes a Boolean predicate, pred, (pred returns true/false)
  and returns a lifted predicate, which takes the same arguments,
  but returns a heavy-bool"
  (lambda (&rest args)
    (heavy-bool (apply pred args)
                :args args)))
           

(defun +annotate-reasons (hb reasons-plist)
  (make-instance (class-of hb)
                 :reason (cons reasons-plist (reason hb))))


(defgeneric heavy-bool (bool &rest reasons-plist))

(defmethod heavy-bool ((bool heavy-bool) &rest reasons-plist)
  (if reasons-plist
   (+annotate-reasons bool reasons-plist)
   bool))

(defmethod heavy-bool ((bool null) &rest reason-plist)
  (if reason-plist
    (make-instance 'heavy-false :reason (list reason-plist))
    (make-instance 'heavy-false)))

(defmethod heavy-bool ((bool (eql t)) &rest reason-plist)
  (if reason-plist
    (make-instance 'heavy-true :reason (list reason-plist))
    (make-instance 'heavy-true)))

(defmethod heavy-bool ((unknown t) &rest reason-plist)
  (error "cannot create heavy-bool from ~A" unknown))


(defvar *heavy-true* (heavy-bool t))
(defvar *heavy-false* (heavy-bool nil))

(defgeneric +not (hb))
(defmethod +not ((hb heavy-true))
  (make-instance 'heavy-false :reason (reason hb)))

(defmethod +not ((hb heavy-false))
  (make-instance 'heavy-true :reason (reason hb)))

(defmacro +if (condition consequent &optional alternative)
  `(if (bool ,condition)
       ,consequent
       ,alternative))

(defmacro +and (&rest clauses)
  (case (length clauses)
    ((0)
     '*heavy-true*)
    ((1)
     (car clauses))
    (t
     (let ((v (gensym "and"))
           (head (car clauses))
           (tail (cdr clauses)))
       `(let ((,v ,head))
          (+if ,v
               (+and ,@tail)
               ,v))))))
     
(defmacro +or (&rest clauses)
  (case (length clauses)
    ((0)
     '*heavy-false*)
    ((1)
     (car clauses))
    (t
     (let ((v (gensym "or"))
           (head (car clauses))
           (tail (cdr clauses)))
       `(let ((,v ,head))
          (+if ,v
               ,v
               (+or ,@tail)))))))

(defmacro +implies (a b)
  `(+or (+not ,a) ,b))

(defmacro +implied-by (b a)
  `(+or ,b
        (+not ,a)))

(defun +iff (a b)
  (+and (+implies a b)
        (+implied-by a b)))

(defun +annotate (hb &rest reasons)
  ;; reasons is a property list
  (+annotate-reasons hb reasons))

(defun +tag (hb key)
  (+annotate hb key (bool hb)))

(defun +annotate-true (hb &rest reasons)
  (+if hb
       (+annotate-reasons hb reasons)
       hb))

(defun +annotate-false (hb &rest reasons)
  (+if hb
       hb
       (+annotate-reasons hb reasons)))

(defun forall (v predicate coll)
  (reduce (lambda (hb item)
            (let ((this (heavy-bool (funcall predicate item))))
              (if (bool this)
                  hb
                  (return-from forall (+annotate this
                                                 :witness item
                                                 :var v)))))
          coll
          :initial-value *heavy-true*))

(defun exists (v predicate coll)
  (+not (forall v
                (lambda (x) (+not (heavy-bool (funcall predicate x))))
                coll)))

(defmacro +forall ((var coll &rest pairs) &body body)
  (if (null pairs)
      `(forall ',var (lambda (,var) ,@body) ,coll)
      `(forall ',var (lambda (,var)
                       (+forall ,pairs ,@body))
               ,coll)))

(defmacro +exists ((var coll &rest pairs) &body body)
  (if (null pairs)
      `(exists ',var (lambda (,var) ,@body) ,coll)
      `(exists ',var (lambda (,var) 
                       (+exists ,pairs ,@body))
               ,coll)))

(defmacro +assert (val &rest args)
  `(assert (bool ,val) ,@ args))

(defun find-reason (hb key &optional default)
  "search in reasons (a list of plists) for a plist having key=`key`,
   and return the associated value.  If no such plist is found,
   `default` is returned"
  (let ((no-key '(:no-key)))
    (loop :for plist :in (reason hb)
          :unless (eq no-key (getf plist key no-key))
            :do (return-from find-reason (getf plist key)))
    default))

(defun find-witness (hb &optional default)
  "Find the first :witness key in (reason hb) which is a list of plists."
  (find-reason hb :witness default))
