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



