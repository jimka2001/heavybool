;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(asdf:defsystem :heavy-bool-examples
  :version (:read-file-form "version.lisp")
  :author "Jim Newton"
  :description "Examples of heavy-booleans"
  :license "MIT"
  :depends-on (:heavy-bool)
  :components
  ((:module "src"
    :components
    ((:file "heavy-bool-examples-package")
     (:file "magma")
     (:file "dyn-magma" :depends-on ("magma"))
     (:file "mod-p" :depends-on ("magma"))
     (:file "relations")
     (:file "gaussian-int" )
     ))))
