(ns lab5.integral)

(defn trapezoid-area
  "Найти площадь трапеции функции с основаниями f(a) и f(b) и высотой (b - a)"
  [f a b] (* (/ (- b a) 2) (+ (f a) (f b)))
)

(defn recursive-integrate
  "Рекурсивное интегрирование функции f, h - шаг рекурсии"
  [f h] (
    fn [mem-integrate n] (
      let [inner-integrate (fn [n] (mem-integrate mem-integrate n))] (
        let [previous_n (- n 1) a (* previous_n h) b (* n h)] (
          if
            (= n 1) (trapezoid-area f 0 h)
            (+ (trapezoid-area f a b) (inner-integrate previous_n))
        )
      )
    )
  )
)

(defn do-integrate
  "Функция возвращает функцию, которая вычисляет интеграл функции f
   методом трапеций с шагом h в точке x = n * h"
  [f h] (
    let [mem-integrate (memoize (recursive-integrate f h))] (
      partial mem-integrate mem-integrate
    )
  )
)

(defn integrate-from-origin
  "Проинтегрировать функцию f методом трапеций с постоянным шагом от 0 до x"
  [f h x] (
    let [
      n (int (Math/floor (/ x h)))
      a (* n h)
      b x
      integrate-n (do-integrate f h)
    ] (
      + (trapezoid-area f a b) (integrate-n n)
    )
  )
)

(defn integrate
  "Проинтегрировать функцию f методом трапеций с постоянным шагом h от 0 до x"
  [f h] (
    let [foo (partial (partial integrate-from-origin f) h)] (
      fn [x] (foo x)
    )
  )
)

(println ((integrate (fn [x] (* x x)) 1) 2))
