(ns els2025
  "Examples to support els 2025 paper submission"
)

(def example1 []
  (let [M [1 2 3 4 5]
        op (fn [x y] (- x y))]
    (some (fn [x]
            (some (fn [y] (when (not= (op x y) (op y x)) [x y]))
                  M))
          M)))

