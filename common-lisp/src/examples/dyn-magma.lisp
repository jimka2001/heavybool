;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool-examples)

(defclass dyn-magma (magma)
  ((gen :initarg :gen :type (function () t))
   (op :initarg :op :type (function (t t) t))
   (is-member :initarg :is-member :type (function (t) t))))

(defmethod gen ((md dyn-magma))
  (funcall (slot-value md 'gen)))
  
(defmethod op ((md dyn-magma) a b)
  (funcall (slot-value md 'op) a b))

(defmethod is-member ((md dyn-magma) a)
  (+tag (heavy-bool (funcall (slot-value md 'is-member) a)
                    :a a)
        :member))

