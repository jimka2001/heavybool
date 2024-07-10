;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :heavy-bool-test
;;  (:shadowing-import-from :rte "TEST")
  (:use :cl :heavy-bool :heavy-bool-examples :scrutiny :alexandria))

(in-package :heavy-bool-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :heavy-bool
                      :package-into :heavy-bool-test)
  (shadow-all-symbols :package-from :heavy-bool-examples
                      :package-into :heavy-bool-test))

(defun test ()
  (run-package-tests :heavy-bool-test))

(defun test-serialize (hb)
  (cons (bool hb) (reason hb)))

;; (heavy-bool (heavy-bool t :x 100))

(define-test t-constructor
  (assert-true (eql (class-of (heavy-bool t))
                    (find-class 'heavy-true)))
  (assert-true (eql (class-of (heavy-bool nil))
                    (find-class 'heavy-false)))
  (let ((hb1 (heavy-bool t :x 100))
        (hb2 (heavy-bool nil :x 200)))
    (assert-true (eq hb1 (heavy-bool hb1)))
    (assert-true (eq hb2 (heavy-bool hb2)))
    (assert-true (equal (test-serialize (heavy-bool hb1 :y 300))
                        '(t :y 300 :x 100)))
    (assert-true (equal (test-serialize (heavy-bool hb2 :y 300))
                        '(nil :y 300 :x 200)))))




(define-test t-and
  (let ((hb (+and (+forall x (iota 10 :start 1 :step 3)
                    (heavy-bool (oddp x) :forall t))
                  (+exists x (iota 10 :start 1 :step 3)
                    (heavy-bool (oddp x) :exists t)))))
    (assert-true (equal (bool hb) nil))
    (assert-true (equal (reason hb) '(:witness 4 :tag x :forall t)))))

(define-test t-or
  (let ((hb (+or (+forall x (iota 10 :start 1 :step 3)
                   (heavy-bool (oddp x) :forall t))
                 (+exists x (iota 10 :start 1 :step 3)
                   (heavy-bool (oddp x) :exists t)))))
    (assert-true (equal (bool hb) t))
    (assert-true (equal (reason hb) '(:witness 1 :tag x :exists t)))))


(define-test t-forall
  (assert-true
   (equal (test-serialize (+forall x '(1 2 3)
                            (if (> x 0)
                                (heavy-bool t :reason :works)
                                (heavy-bool nil :reason :fails))))
          '(t)))
  (assert-true
   (equal (test-serialize (+forall x '(1 2 3)
                            (if (<= x 1)
                                (heavy-bool t :reason :works)
                                (heavy-bool nil :reason :fails))))
          '(nil :witness 2 :tag x :reason :fails))))
  


(define-test t-or-2
  (assert-true (equal (test-serialize (+or (heavy-bool nil :reason 1)
                                           (heavy-bool nil :reason 2)))
                      '(nil :reason 2)))
  (assert-true (equal (test-serialize (+or (heavy-bool nil :reason 1)
                                           (heavy-bool nil :reason 2)
                                           (heavy-bool nil :reason 3)))
                      '(nil :reason 3)))
  (assert-true (equal (test-serialize (+or (heavy-bool nil :reason 1)
                                           (heavy-bool t :reason 2)
                                           (heavy-bool nil :reason 3)))
                      '(t :reason 2))))


(define-test t-and-2
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)
                                            (heavy-bool nil :reason 2)))
                      '(nil :reason 1)))
  (assert-true (equal (test-serialize (+and (heavy-bool t :reason 1)
                                            (heavy-bool t :reason 2)))
                      '(t :reason 2)))
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)
                                            (heavy-bool t :reason 2)))
                      '(nil :reason 1)))

  (assert-true (equal (test-serialize (+and (heavy-bool t :reason 1)
                                            (heavy-bool nil :reason 2)))
                      '(nil :reason 2)))
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)
                                            (heavy-bool t :reason 2)))
                      '(nil :reason 1)))
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)
                                            (heavy-bool nil :reason 2)
                                            (heavy-bool nil :reason 3)))
                      '(nil :reason 1)))
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)
                                            (heavy-bool t :reason 2)
                                            (heavy-bool nil :reason 3)))
                      '(nil :reason 1))
               )
  (assert-true (equal (test-serialize (+and (heavy-bool t :reason 1)
                                            (heavy-bool t :reason 2)
                                            (heavy-bool t :reason 3)))
                      '(t :reason 3)))
  (assert-true (equal (test-serialize (+and (heavy-bool t :reason 1)))
                      '(t :reason 1)))
  (assert-true (equal (test-serialize (+and (heavy-bool nil :reason 1)))
                      '(nil :reason 1)))
  (assert-true (equal (test-serialize (+and))
                      '(t)
                      )))
                        
;;(run-package-tests '(:heavy-bool-test) :break-on-error t)
