;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-examples)

(defclass mod-p (magma)
  ((p :initarg :p :type integer)))

(defmethod gen ((mp mod-p))
  (gen-list-finite (1- (slot-value mp 'p))))

(defmethod is-equiv ((mp mod-p) a b)
  (+tag (heavy-bool (= (mod a (slot-value mp 'p))
                       (mod b (slot-value mp 'p)))
                    :p (slot-value mp 'p))
        :equiv-mod-p))

(defmethod is-member ((mp mod-p) a)
  (let ((p (slot-value mp 'p)))
    (+tag 
     (+annotate 
      (+and (+annotate-false (heavy-bool (>= a 0)) :reason "a < 0")
            (+annotate-false (heavy-bool (< a p)) :reason "a >= p"))
      :a a :p p)
     :is-member)))

(defclass addition-mod-p (mod-p) ())

(defmethod op ((amp addition-mod-p) a b)
  (assert (integerp a))
  (assert (integerp b))
  (mod (+ a b) (slot-value amp 'p)))

(defclass multiplication-mod-p (mod-p) ())

(defmethod gen ((mmp multiplication-mod-p))
  (remove 0 (call-next-method)))

(defmethod is-member ((mmp multiplication-mod-p) a)
  (+and (+annotate-false (heavy-bool (< 0 a)) :reason "a <= 0")
        (call-next-method)))

(defmethod op ((mmp multiplication-mod-p) a b)
  (mod (* a b) (slot-value mmp 'p)))
