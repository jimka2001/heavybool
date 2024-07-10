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

(define-test mod-p-primes
  (loop :for p in '(2 3 5 7 11)
        :do (let ((g (make-instance 'multiplication-mod-p :p p)))
              (labels ((inv (a)
                         (loop :for b :from 1 :below p
                               :do (if (= 1 (mod (* a b) p))
                                       (return-from inv (values b t))))
                         (values nil nil)))
                (assert-true (bool (is-group g 1 #'inv))
                             :tag 33)))))

(define-test mod-p-composites
  (loop :for p in '(4 6 8 9 10)
        :do (let ((g (make-instance 'multiplication-mod-p :p p)))
              (labels ((inv (a)
                         (loop :for b :from 1 :below p
                               :do (if (= 1 (mod (* a b) p))
                                       (return-from inv (values b t))))
                         (values nil nil)))
                (assert-true (bool (+not (is-group g 1 #'inv)))
                             :tag 33)))))
                       
