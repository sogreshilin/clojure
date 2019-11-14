(ns lab8.operations
  (:require
    [lab8.constants :refer :all]
    [lab8.variables :refer :all]
  )
)

(defn conjunction "Конструктор конъюнкции" [expr & rest]
  (cons ::conj (cons expr rest))
)

(defn conjunction? "Проверка типа для конъюнкции" [expr]
  (= (first expr) ::conj)
)

(defn disjunction "Конструктор дизъюнкции" [expr & rest]
  (cons ::disj (cons expr rest))
)

(defn disjunction? "Проверка типа для дизъюнкции" [expr]
  (= (first expr) ::disj)
)

(defn implication "Конструктор имликации" [cause effect]
  (cons ::impl (list cause effect))
)

(defn implication? "Проверка типа для имликации" [expr]
  (= (first expr) ::impl)
)

(defn negation "Конструктор отрицания" [expr]
  (cons ::neg (list expr))
)

(defn negation? "Проверка типа для отрицания" [expr]
  (= (first expr) ::neg)
)

(defn operation "Получение операции выражения" [expr]
  (first expr)
)

(defn args "Получение аргументов выражения" [expr]
  (rest expr)
)
