(ns lab8.constants)

(defn constant "Конструктор константы" [value]
  {:pre [(or (= value 0) (= value 1))]}
  (list ::const value)
)

(defn constant? "Проверка типа для константы" [expr]
  (= (first expr) ::const)
)

(defn constant-value "Получение значения константы" [c]
  (second c)
)

(defn same-constants? "Сравнение констант" [c1 c2]
  (and
    (constant? c1)
    (constant? c2)
    (= (constant-value c1) (constant-value c2))
  )
)
