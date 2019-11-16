(ns lab8.core
  (:require
    [lab8.constants :refer :all]
    [lab8.variables :refer :all]
    [lab8.operations :refer :all]
    [lab8.serialization :refer :all]
  )
)

(defn get-rid-of-implication "Избавление от импликации" [expr]
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

(defn push-negation "Протаскивание отрицания до пропозициональных переменных" [expr]
  (cond
    (constant? expr) expr
    (variable? expr) expr
    (negation? expr)
      (cond
        (constant? (first (args expr)))
          (cond
            (same-constants? (first (args expr)) (constant 1)) (constant 0)
            (same-constants? (first (args expr)) (constant 0)) (constant 1)
          )
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

(defn distribute-conjunction "Применение свойства дистрибутивности конъюнкции" [expr]
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

(defn distribute "Применение свойства дистрибутивности конъюнкции и дизъюнкции" [expr]
  (cond
    (disjunction? expr) (apply disjunction (map distribute (args expr)))
    (conjunction? expr) (distribute-conjunction expr)
    :default expr
  )
)

(declare associate)

(defn associate-conjunction "Применение свойства ассоциативности конъюнкции" [expr]
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

(defn associate-disjunction "Применение свойства ассоциативности дизъюнкции" [expr]
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

(defn associate "Применение свойства ассоциативности конъюнкции и дизъюнкции" [expr]
  (cond
    (disjunction? expr) (associate-disjunction expr)
    (conjunction? expr) (associate-conjunction expr)
    :default expr
  )
)

(defn filter-zeros-from-conjunction "Конъюнкция с нулем равна нулю" [expr]
  (cond
    (conjunction? expr)
      (let [
        new-args (filter (fn [x] (not (same-constants? (constant 0) x))) (args expr))
        size (count new-args)
      ] (
          cond
            (= (count new-args) (count (args expr))) expr
            :default (constant 0)
        )
      )
    :default expr
  )
)

(defn filter-ones-from-conjunction "Конъюнкция выражения с единицей равна самому выражению" [expr]
  (cond
    (conjunction? expr)
      (let [
        new-args (filter (fn [x] (not (same-constants? (constant 1) x))) (args expr))
        size (count new-args)
      ] (
          cond
            (= size 0) (constant 1)
            (= size 1) (first new-args)
            :default (apply conjunction new-args)
        )
      )
    :default expr
  )
)

(defn filter-ones-from-disjunction "Дизъюнкция с единицей равна единице" [expr]
  (cond
    (disjunction? expr)
      (let [
        new-args (filter (fn [x] (not (same-constants? (constant 1) x))) (args expr))
        size (count new-args)
      ] (
          cond
            (= (count new-args) (count (args expr))) expr
            :default (constant 1)
        )
      )
    :default expr
  )
)

(defn filter-zeros-from-disjunction "Дизъюнкция выражения с нулем равна самому выражению" [expr]
  (cond
    (disjunction? expr)
      (let [
        new-args (filter (fn [x] (not (same-constants? (constant 0) x))) (args expr))
        size (count new-args)
      ] (
          cond
            (= size 0) (constant 0)
            (= size 1) (first new-args)
            :default (apply disjunction new-args)
        )
      )
    :default expr
  )
)

(defn get-rid-of-constants "Избавиться от лишних констант" [expr]
  (cond
    (constant? expr) expr
    (variable? expr) expr
    (negation? expr) (negation (get-rid-of-constants (first (args expr))))
    (conjunction? expr) (filter-ones-from-conjunction (filter-zeros-from-conjunction expr))
    (disjunction? expr) (filter-zeros-from-disjunction (filter-ones-from-disjunction expr))
  )
)

(defn dnf "Привести формулу к ДНФ" [expr]
  (get-rid-of-constants (associate (distribute (push-negation (get-rid-of-implication expr)))))
)

(defn apply-values "Подставить значения в выражение" [expr values]
  (let [do-apply-values (fn [expression] (apply-values expression values))]
    (cond
      (constant? expr) expr
      (variable? expr) (get values expr expr)
      (negation? expr) (negation (do-apply-values (first (args expr))))
      (conjunction? expr) (apply conjunction (map do-apply-values (args expr)))
      (disjunction? expr) (apply disjunction (map do-apply-values (args expr)))
      (implication? expr) (apply implication (map do-apply-values (args expr)))
    )
  )
)

(defn by-step [expr]
  (println (to-string (get-rid-of-implication expr)))
  (println (to-string (push-negation (get-rid-of-implication expr))))
  (println (to-string (distribute (push-negation (get-rid-of-implication expr)))))
  (println (to-string (associate (distribute (push-negation (get-rid-of-implication expr))))))
  (println (to-string (get-rid-of-constants (associate (distribute (push-negation (get-rid-of-implication expr)))))))
)
