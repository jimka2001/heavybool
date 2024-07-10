;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(asdf:defsystem :heavy-bool-test
  :version (:read-file-form "version.lisp")
  :author "Jim Newton"
  :description "Test cases for heavy-bool"
  :license "MIT"
  :depends-on (:scrutiny
               :heavy-bool
               :heavy-bool-examples
               )
  :components
  ((:module "test"
    :components
    ((:file "test-heavy-bool")
     (:file "test-relations")
     (:file "test-magma")
     (:file "test-mod-p")
     ))))
