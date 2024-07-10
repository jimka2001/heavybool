;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :heavy-bool :package-into :heavy-bool-test))


(define-test t-symmetric-19
  (assert-false (bool (is-symmetric (constantly (iota 20 :start 1))
                                    #'<))
                :tag 18)
  (assert-true (bool (is-symmetric (constantly (iota 20 :start 1))
                                   #'=))
               :tag 19)
  (assert-true (bool (is-symmetric (constantly (iota 20 :start 1))
                                   #'/=))
               :tag 20))

(define-test t-equivalence-19
  (assert-false (bool (is-equivalence (constantly (iota 20 :start 1))
                                #'<))))

(define-test t-relations-1
  (assert-false (bool (is-reflexive (lambda ()
                                      (iota 10))
                                    (lambda (a b)
                                      (heavy-bool (< a b)))))))

(define-test t-relations-2
  (assert-true  (bool (is-reflexive (lambda ()
                                      (iota 10))
                                    (lambda (a b)
                                      (heavy-bool (= a b)))))))

(define-test t-relations-equivalence
  (assert-true  (bool (is-equivalence (lambda ()
                                        (iota 10))
                                      (lambda (a b)
                                        (heavy-bool (= a b))))))
  (assert-false  (bool (is-equivalence (lambda ()
                                         (iota 10))
                                       (lambda (a b)
                                         (heavy-bool (/= a b))))))
  (assert-false  (bool (is-equivalence (lambda ()
                                         (iota 10))
                                       (lambda (a b)
                                         (heavy-bool (< a b)))))))


(define-test t-relations-strict-partial-order
  (assert-false  (bool (is-strict-partial-order (lambda ()
                                         (iota 10))
                                       (lambda (a b)
                                         (heavy-bool (= a b))))))
  (assert-false  (bool (is-strict-partial-order (lambda ()
                                         (iota 10))
                                       (lambda (a b)
                                         (heavy-bool (/= a b))))))
  (assert-true  (bool (is-strict-partial-order (lambda ()
                                        (iota 10))
                                      (lambda (a b)
                                        (heavy-bool (< a b))))))
  (assert-false  (bool (is-strict-partial-order (lambda ()
                                         (iota 10))
                                       (lambda (a b)
                                         (heavy-bool (<= a b)))))))

(define-test symmetric-75
  (assert-false (bool (is-symmetric (constantly (iota 20)) #'<)))
  (assert-false (bool (is-symmetric (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-symmetric (constantly (iota 20)) #'=))))

(define-test assymmetric-80
  ;; if a relates to b, then b does not relate to a
  (assert-true (bool (is-asymmetric (constantly (iota 20)) #'<)))
  (assert-false (bool (is-asymmetric (constantly (iota 20)) #'<=)))
  (assert-false (bool (is-asymmetric (constantly (iota 20)) #'=))))

(define-test antisymmetric-86
  ;; if a relates to b and b relates to a then a == b
  (assert-true (bool (is-antisymmetric (constantly (iota 20)) #'<)))
  (assert-true (bool (is-antisymmetric (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-antisymmetric (constantly (iota 20)) #'=)))
  (assert-false (bool (is-antisymmetric (constantly (iota 20)) #'/=))))

(define-test transitive-93
  (assert-true (bool (is-transitive (constantly (iota 20)) #'<)))
  (assert-true (bool (is-transitive (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-transitive (constantly (iota 20)) #'=)))
  (assert-false (bool (is-transitive (constantly (iota 20)) #'/=))))


(define-test reflexive-100
  (assert-true (bool (is-reflexive (constantly (iota 20)) #'=)))
  (assert-true (bool (is-reflexive (constantly (iota 20)) #'<=)))
  (assert-false (bool (is-reflexive (constantly (iota 20)) #'<)))
  (assert-false (bool (is-reflexive (constantly (iota 20)) #'/=))))

(define-test irreflexive-106
  ;; no element relates to itself
  (assert-false (bool (is-irreflexive (constantly (iota 20)) #'=)))
  (assert-false (bool (is-irreflexive (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-irreflexive (constantly (iota 20)) #'<))))

(define-test equivalence-112
  (assert-false (bool (is-equivalence (constantly (iota 20)) #'<)))
  (assert-false (bool (is-equivalence (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-equivalence (constantly (iota 20)) #'=))))

(define-test partial-order-117
  (assert-true (bool (is-partial-order (constantly (iota 20)) #'<=)))
  (assert-false (bool (is-partial-order (constantly (iota 20)) #'<)))
  (assert-true (bool (is-partial-order (constantly (iota 20)) #'=)))
  (assert-false (bool (is-partial-order (constantly (iota 20)) #'/=))))

(define-test strict-partial-order-123
  (assert-true (bool (is-strict-partial-order (constantly (iota 20)) #'<)))
  (assert-false (bool (is-strict-partial-order (constantly (iota 20)) #'<=)))
  (assert-false (bool (is-strict-partial-order (constantly (iota 20)) #'=)))
  (assert-false (bool (is-strict-partial-order (constantly (iota 20)) #'/=))))

(define-test connected-129
  (assert-true (bool (is-connected (constantly (iota 20)) #'<)))
  (assert-true (bool (is-connected (constantly (iota 20)) #'>)))
  (assert-true (bool (is-connected (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-connected (constantly (iota 20)) #'>=)))
  (assert-false (bool (is-connected (constantly (iota 20)) #'=)))
  (assert-true (bool (is-connected (constantly (iota 20)) #'/=))))

(define-test strongly-connected-137
  (assert-false (bool (is-strongly-connected (constantly (iota 20)) #'<)))
  (assert-false (bool (is-strongly-connected (constantly (iota 20)) #'>)))

  (assert-true (bool (is-strongly-connected (constantly (iota 20)) #'<=)))
  (assert-true (bool (is-strongly-connected (constantly (iota 20)) #'>=)))

  (assert-false (bool (is-strongly-connected (constantly (iota 20)) #'=)))
  (assert-false (bool (is-strongly-connected (constantly (iota 20)) #'/=))))
