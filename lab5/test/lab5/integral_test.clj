(ns lab5.integral-test
  (:require [clojure.test :refer :all]
            [lab5.integral :refer :all]))

(deftest trapezoid-area-test
  (testing "Вычислить площадь трапеции"
    (is (= 6 (trapezoid-area (fn [x] x) 2 4)))
    (is (= 5/2 (trapezoid-area (fn [x] (* x x)) 1 2)))
    (is (= 16 (trapezoid-area (fn [x] (* x x)) -2 2)))
  )
)

(deftest integrate-test
  (testing "Проинтегрировать функцию методом трапеций"
    (is (= 25/2 ((integrate (fn [x] x) 1) 5)))
    (is (= 19/2 ((integrate (fn [x] (* x x)) 1) 3)))
    (is (= 25/2 ((integrate (fn [x] x) 1/10) 5)))
    (is (= 1801/200 ((integrate (fn [x] (* x x)) 1/10) 3)))
    (let
      [
        function (fn [x] 1)
        antiderivative (fn [x] x)
      ] (is (= (antiderivative 10) ((integrate function 1) 10)))
    )
  )
)
