(ns util
  (:require  [clojure.test :refer [is testing testing-contexts-str *testing-contexts*]]
             [clojure.pprint :refer [cl-format]]))

(defn tails
  "Return a lazy list of tails of the given collection."
  [coll]
  (lazy-seq (if (empty? coll)
              ()
              (cons coll (tails (rest coll))))))

(defn time-call
  "Evaluates thunk and returns a 2-vector [value elapsed-time]
  where value is the return value of the thunk and elapsed-time
  is the number of nano it took evaluating the function."
  [thunk]
  (let [start (. System (nanoTime))
        value (thunk)
        stop (. System (nanoTime))]
    [value (- stop start)]))


(defn member
  "Determines whether the given target is an element of the given sequence (or given set)."
  [target items]
  (boolean (cond
             (empty? items) false
             (nil? target) (some nil? items)
             (false? target) (some false? items)
             (set? items) (contains? items target)
             :else (reduce (fn [_acc item]
                             (if (= item target)
                               (reduced true)
                               false)) false items))))

(defn almost-equal 
  "Returns a binary function which can be used to test whether two
  given arguments are within a given tolerance of each other."
  [tolerance]
  (assert (float? tolerance))
  (fn [x y]
    (assert (number? x))
    (assert (number? y))
    (or (= x y)
        (<= (abs (- x y)) tolerance))))

(defn almost-equal-seq
  "Returns a binary function which can be used to test whether two
  given sequence arguments are element-wise within a given tolerance of each other."
  [tolerance]
  (assert (float? tolerance))
  (let [ae (almost-equal tolerance)]
    (fn [x-seq y-seq]
      (and (= (count x-seq) (count y-seq))
           (every? true? (map ae x-seq y-seq))))))

(defn re-chunk 
  "Given a lazy sequence, change the chunking buffer size to n.
  This code was taken directory from
  https://clojuredocs.org/clojure.core/chunk"
  [n xs]
  (lazy-seq
    (when-let [s (seq (take n xs))]
      (let [cb (chunk-buffer n)]
        (doseq [x s] (chunk-append cb x))
        (chunk-cons (chunk cb) (re-chunk n (drop n xs)))))))

(defn find-if
  "Find the first element in the sequence which makes the predicate true.
  If such an item is found, a singleton list of the item is returned,
  otherwise nil is returned.
  "
  [f col]
  (reduce (fn [_ item] (if (f item)
                           (reduced (list item))
                           nil))
          nil
          col))


(defmacro first-st
  "Return the first element of a collection which causes the `body`
  to be true.  `find-if` is used to attempt prevent the expression
  from evaluating more times than necessary."
  [var col & body]
  `(first (find-if (fn [~var] ~@body) ~col)))

(def ^:dynamic *time-out*
  "Dynamic variable controling the default number of milliseconds
  is used as the time-out used by `testing-with-timeout`."
  (* 2 60 1000)) ;; 2 seconds

(defn call-with-timeout
  "Call the given 0-arty function in a separate thread (via `future`)
  and dereference it with `timeout-ms` as the time-out.
  This function is intended to be called withint a `clojure.test` test
  case (defined by `deftest`). If such a test times out, a test failure
  is registered, by a side-effecing call to `(is ...)`"
  [timeout-ms f]
  (let [timeout-val ::test-timeout
        fut         (future (f))
        val         (deref fut timeout-ms timeout-val)]
    (if (= val timeout-val)
      (do
        (future-cancel fut)
          
        (is (= :success (cl-format nil "~A [~A] Test timed out after ~Ams!"
                                   (testing-contexts-str)
                                   *testing-contexts*
                                   timeout-ms))))
      val)))

(defmacro testing-with-timeout
  "Similar to the `clojure.test` macro `testing` but limits
  a time-out to `*time-out*`."
  [msg & body]
  `(testing ~msg
     (call-with-timeout *time-out*
                        (fn []
                          ~@body))))

(defn type-check 
  "Evaluates to the value of `x` but with a pre-condition
  that (f x) is true."
  [f x]
  {:pre [(f x)]}
  x)

(defn power-set 
  "Given a set of items, `base-set` return a set containing
  all subsets of `base-set`."
  [base-set]
  {:pre [(set? base-set)]}
  (reduce (fn [set-of-sets item]
            (concat set-of-sets
                    (into #{} (for [s set-of-sets]
                                (conj s item)))))

          #{#{}}
          base-set))
(defn power
  "compute the base `b` raised to the power `p`"
  [b p]
  (cond (and (= b 0) (= p 0))
        (throw (ex-info "cannot compute (power 0 0)" {}))

        (= b 0)
        0

        (= p 0)
        1

        (< p 0)
        (recur (/ b) (- p))

        (even? p)
        (recur (* b b) (quot p 2))

        :else
        (* b (power b (dec p)))))
