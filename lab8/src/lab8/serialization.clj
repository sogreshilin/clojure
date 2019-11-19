(ns lab8.serialization
  (:require
    [lab8.constants :refer :all]
    [lab8.variables :refer :all]
    [lab8.operations :refer :all]
  )
)

(declare to-string)

(defn brackets [expr]
  (cond
    (or
      (constant? expr)
      (variable? expr)
    ) (to-string expr)
    (negation? expr) (str "~" (brackets (first (args expr))))
    :default (str "(" (to-string expr) ")")
  )
)

(defn to-string [expr]
  (cond
    (constant? expr) (str (first (args expr)))
    (variable? expr) (subs (str (first (args expr))) 1)
    (negation? expr) (str "~" (brackets (first (args expr))))
    (conjunction? expr) (clojure.string/join "&" (map brackets (args expr)))
    (disjunction? expr) (clojure.string/join "|" (map brackets (args expr)))
    (implication? expr) (clojure.string/join "->" (map brackets (args expr)))
  )
)
