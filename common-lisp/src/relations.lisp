;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :heavy-bool)


(defun is-reflexive (gen <)
  (+annotate
   (+forall x (funcall gen)
     (+annotate-false (funcall < x x)
                      :x x
                      :reason "not symmetric"))
   :tag "reflexive"))

(defun is-symmetric (gen <)
  (+annotate
   (+forall x (funcall gen)
     (+forall y (funcall gen)
       (+annotate-false (+implies (funcall < x y)
                                  (funcall < y x))
                        :x x
                        :y y
                        :reason "not symmetric")))
   :tag "symmetric"))

(defun is-transitive (gen <)
  (+annotate
   (+forall x (funcall gen)
     (+forall y (funcall gen)
       (+implies (funcall < x y)
                 (+forall z (funcall gen)
                   (+implies (funcall < y z)
                             (funcall < x z))))))
   :tag "transitive"))

(defun is-equivalence (gen =)
  (+annotate
   (+and (is-reflexive gen =)
         (is-symmetric gen =)
         (is-transitive gen =))
   :tag "equivalence"))

(defun is-asymmetric (gen <)
  (+annotate
   (+forall x (funcall gen)
     (+forall y (funcall gen)
       (+annotate-false (+implies (funcall < x y)
                                  (+not (funcall < y x)))
                        :x x
                        :y y)))
   :tag "symmetric"))

(defun is-irreflexive (gen <)
  (+annotate (+not (+exists x (funcall gen)
                     (funcall < x x)))
             :tag "irreflexive"))

(defun is-strict-partial-order (gen <)
  (+annotate (+and (is-irreflexive gen <)
                   (is-transitive gen <)
                   (is-asymmetric gen <))
             :tag "strict partial order"))

