(ns karatsuba
  
)

(defn power-of-10 [m]
  (loop [d 1N
         n 0]
    (if (> m d)
      (recur (* 10 d) (inc n))
      n)))

(defn power [b p]
  (cond (= p 0)
        1

        (= p 1)
        b

        (even? p)
        (recur (* b b) (quot p 2))

        :else
        (* b (power b (dec p)))))




(defn mult [AB CD]
  (let [n (max (power-of-10 AB)
               (power-of-10 CD))
        ]
    (letfn [(karatsuba [AB CD n]
              (println [AB CD n])
              (if (< n 3)
                (* AB CD)
                (let [X (power 10N n)
                      n2 (quot n 2)
                      XX (power 10N n2)
                      a (quot AB XX)
                      c (quot CD XX)
                      b (mod AB XX)
                      d (mod CD XX)
                      ac (karatsuba a c n2)
                      bd (karatsuba b d n2)
                      ad+bc (- (karatsuba (+ a b) (+ c d) n2) ac bd)
                      ]
                  (+ (* ac X )
                     (* ad+bc XX )
                     bd))))]
      (karatsuba AB CD n))))

(mult 9876543210N 123456780N)

