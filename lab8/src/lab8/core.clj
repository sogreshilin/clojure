(ns lab8.core
  (:require
    [lab8.constants :refer :all]
    [lab8.variables :refer :all]
    [lab8.operations :refer :all]
    [lab8.serialization :refer :all]
  )
)

(defn get-rid-of-implication [expr]
  (cond
    (constant? expr) expr
    (variable? expr) expr
    (negation? expr) (negation (get-rid-of-implication (first (args expr))))
    (implication? expr) (
      disjunction
      (negation (get-rid-of-implication (nth expr 1)))
      (get-rid-of-implication (nth expr 2))
    )
    (conjunction? expr)
      (apply conjunction (map get-rid-of-implication (args expr)))
    (disjunction? expr)
      (apply disjunction (map get-rid-of-implication (args expr)))
  )
)

(defn push-negation [expr]
  (cond
    (constant? expr) expr
    (variable? expr) expr
    (negation? expr)
      (cond
        (constant? (first (args expr))) expr
        (variable? (first (args expr))) expr
        (negation? (first (args expr))) (first (args (first (args expr))))
        (conjunction? (first (args expr))) (apply disjunction (map push-negation (map negation (args (first (args expr))))))
        (disjunction? (first (args expr))) (apply conjunction (map push-negation (map negation (args (first (args expr))))))
      )
    (conjunction? expr)
      (apply conjunction (map push-negation (args expr)))
    (disjunction? expr)
      (apply disjunction (map push-negation (args expr)))
    :default
      "Something went wrong"
  )
)

(declare distribute)

(defn distribute-conjunction [expr]
  (
    let [
      a (distribute (first (args expr)))
      b (distribute (second (args expr)))
    ] (cond
        (disjunction? a) (disjunction (conjunction (first (args a)) b) (conjunction (second (args a)) b))
        (disjunction? b) (disjunction (conjunction a (first (args b))) (conjunction a (second (args b))))
        :default (conjunction a b)
    )
  )
)

(defn distribute [expr]
  (cond
    (disjunction? expr) (apply disjunction (map distribute (args expr)))
    (conjunction? expr) (distribute-conjunction expr)
    :default expr
  )
)

(declare associate)

(defn associate-conjunction [expr]
  (
    let [
      a (associate (first (args expr)))
      b (associate (second (args expr)))
    ] (cond
        (and (conjunction? a) (conjunction? b)) (apply conjunction (concat (args a) (args b)))
        (conjunction? a) (apply conjunction (concat (args a) (list b)))
        (conjunction? b) (apply conjunction (concat (list a) (args b)))
        :default (conjunction a b)
    )
  )
)

(defn associate-disjunction [expr]
  (
    let [
      a (associate (first (args expr)))
      b (associate (second (args expr)))
    ] (cond
        (and (disjunction? a) (disjunction? b)) (apply disjunction (concat (args a) (args b)))
        (disjunction? a) (apply disjunction (concat (args a) (list b)))
        (disjunction? b) (apply disjunction (concat (list a) (args b)))
        :default (disjunction a b)
    )
  )
)

(defn associate [expr]
  (cond
    (disjunction? expr) (associate-disjunction expr)
    (conjunction? expr) (associate-conjunction expr)
    :default expr
  )
)

(defn dnf [expr] (associate (distribute (push-negation (get-rid-of-implication expr)))))
