;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defclass heavy-bool () ())
(defvar heavy-true)
(defvar heavy-false)
(defun forall () ())
(defmacro +forall () ())
(defmacro +exists () ())
(defmacro +and)
(defmacro +or)
(defmacro +if)
(defmacro +implies)
(defmacro +implied-by)
(defun +to-bool)

(defun +conj)
(defun +conj-true)
(defun +conj-false)
(defun +annotate)
