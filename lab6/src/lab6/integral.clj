(ns lab6.integral)

(defn trapezoid-area
  "Найти площадь трапеции функции с основаниями f(a) и f(b) и высотой (b - a)"
  [f a b] (* (/ (- b a) 2) (+ (f a) (f b)))
)

(defn make-integral-sequence
  "Получить последовательность значений интеграла,
   вычисленного методом трапеций, в узлах разбиения"
   [f h] (
       iterate
       (fn [[n acc]] (
           let [
             next_n (inc n)
             a (* h n)
             b (* h next_n)
             next_acc (+ acc (trapezoid-area f a b))
          ] [next_n next_acc]
         )
       )
       [0 0]
   )
)

(defn integrate
  [f h] (let [integral-sequence (make-integral-sequence f h)] (
      fn [x] (
        let [
          n (int (Math/floor (/ x h)))
          a (* n h)
          b x

        ] (+ (trapezoid-area f a b) (second (nth integral-sequence n)))
      )
    )
  )
)
