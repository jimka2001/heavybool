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
