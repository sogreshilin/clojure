(ns lab8.variables)

(defn variable "Конструктор переменной" [name]
  {:pre [(keyword? name)]}
  (list ::var name)
)

(defn variable? "Проверка типа для переменной" [expr]
  (= (first expr) ::var)
)

(defn variable-name "Получение названия переменной" [v]
  (second v)
)

(defn same-variables? "Сравнение переменных" [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1) (variable-name v2))
  )
)
