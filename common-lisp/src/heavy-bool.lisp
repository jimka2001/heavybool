;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool)

(defclass heavy-bool ()
  ((bool :initarg :bool :reader bool)
   (reason :type list :initarg :reason :reader reason)))

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


(defun +annotate-reasons (hb reasons)
  (make-instance (class-of hb)
                 :reason (append reasons (reason hb))))


(defgeneric heavy-bool (bool &rest reasons))

(defmethod heavy-bool ((bool heavy-bool) &rest reasons)
  (if reasons
   (+annotate-reasons bool reasons)
   bool))

(defmethod heavy-bool ((bool null) &rest reasons)
  (make-instance 'heavy-false :reason reasons))

(defmethod heavy-bool ((bool t) &rest reasons)
  (make-instance 'heavy-true :reason reasons))

(defvar *heavy-true* (heavy-bool t))
(defvar *heavy-false* (heavy-bool nil))

(defun +not (hb)
  (apply #'heavy-bool (not (bool hb)) (reason hb)))

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

(defun forall (tag predicate coll)
  (reduce (lambda (hb item)
            (let ((this (heavy-bool (funcall predicate item))))
              (if (bool this)
                  hb
                  (return-from forall (+annotate this
                                                 :witness item
                                                 :tag tag)))))
          coll
          :initial-value *heavy-true*))

(defun exists (tag predicate coll)
  (+not (forall tag
                (lambda (x) (+not (heavy-bool (funcall predicate x))))
                coll)))

(defmacro +forall (var coll &body body)
  `(forall ',var (lambda (,var) ,@body) ,coll))

(defmacro +exists (var coll &body body)
  `(exists ',var (lambda (,var) ,@body) ,coll))
