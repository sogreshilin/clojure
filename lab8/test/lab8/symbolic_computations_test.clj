(ns lab8.symbolic-computations-test
  (:require
    [clojure.test :refer :all]
    [lab8.constants :refer :all]
    [lab8.variables :refer :all]
    [lab8.operations :refer :all]
    [lab8.core :refer :all]
    [lab8.serialization :refer :all]
  )
)

(deftest constant-and-variable-test
  (testing "Базовые логические выражения"
    (is (constant? (constant 1)))
    (is (= 1 (constant-value (constant 1))))
    (is (variable? (variable :x)))
    (is (= :x (variable-name (variable :x))))
    (is (same-variables? (variable :x) (variable :x)))
    (is (not (same-variables? (variable :x) (variable :y))))
  )
)

(deftest operations-test
  (testing "Операции над логическими выражениями"
    (is (conjunction? (conjunction (variable :x) (variable :y))))
    (is
      (=
        (list (variable :x) (variable :y))
        (args (conjunction (variable :x) (variable :y)))
      )
    )
    (is (disjunction? (disjunction (variable :x) (conjunction (variable :y) (variable :z)))))
    (is
      (=
        (list (variable :x) (conjunction (variable :y) (variable :z)))
        (args (disjunction (variable :x) (conjunction (variable :y) (variable :z))))
      )
    )
    (is (implication? (implication (variable :x) (disjunction (variable :y) (variable :z)))))
    (is
      (=
        (list (variable :x) (disjunction (variable :y) (variable :z)))
        (args (implication (variable :x) (disjunction (variable :y) (variable :z))))
      )
    )
    (is (negation? (negation (constant 1))))
    (is (= (args (negation (constant 1))) (list (constant 1))))
    (is (same-constants? (constant 1) (first (args (negation (constant 1))))))
  )
)

(deftest to-string-test
  (testing "Сериализация"
    (is (= "1" (to-string (constant 1))))
    (is (= "x" (to-string (variable :x))))
    (is (= "~1" (to-string (negation (constant 1)))))
    (is (= "~x" (to-string (negation (variable :x)))))
    (is (= "~~1" (to-string (negation (negation (constant 1))))))
    (is (= "~~x" (to-string (negation (negation (variable :x))))))
    (is (= "0&1" (to-string (conjunction (constant 0) (constant 1)))))
    (is (= "~0&1" (to-string (conjunction (negation (constant 0)) (constant 1)))))
    (is (= "~~0&1" (to-string (conjunction (negation (negation (constant 0))) (constant 1)))))
    (is (= "0|1" (to-string (disjunction (constant 0) (constant 1)))))
    (is (= "~0|1" (to-string (disjunction (negation (constant 0)) (constant 1)))))
    (is (= "~~0|1" (to-string (disjunction (negation (negation (constant 0))) (constant 1)))))
    (is (= "~(0&1)" (to-string (negation (conjunction (constant 0) (constant 1))))))
    (is (= "~(~0&1)" (to-string (negation (conjunction (negation (constant 0)) (constant 1))))))
    (is (= "~(~~0&1)" (to-string (negation (conjunction (negation (negation (constant 0))) (constant 1))))))
    (is (= "~(0|1)" (to-string (negation (disjunction (constant 0) (constant 1))))))
    (is (= "~(~0|1)" (to-string (negation (disjunction (negation (constant 0)) (constant 1))))))
    (is (= "~(~~0|1)" (to-string (negation (disjunction (negation (negation (constant 0))) (constant 1))))))
    (is (= "(a|b)&(c|d)" (to-string (conjunction (disjunction (variable :a) (variable :b)) (disjunction (variable :c) (variable :d))))))
    (is (= "a->b" (to-string (implication (variable :a) (variable :b)))))
    (is (= "~a->~b" (to-string (implication (negation (variable :a)) (negation (variable :b))))))
  )
)

(deftest get-rid-of-implication-test
  (testing "Избавление от имликации"
    (is (= "~a|b" (to-string (get-rid-of-implication (implication (variable :a) (variable :b))))))
    (is (= "~~a|~b" (to-string (get-rid-of-implication (implication (negation (variable :a)) (negation (variable :b)))))))
    (is (= "~(~a|1)|(~b|0)" (to-string (get-rid-of-implication (implication (implication (variable :a) (constant 1)) (implication (variable :b) (constant 0)))))))
    (is (= "~(~(~(~a|b)|c)|d)|e" (to-string (get-rid-of-implication (implication (implication (implication (implication (variable :a) (variable :b)) (variable :c)) (variable :d)) (variable :e) )))))
    (is (= "~(a&b)|(~c|d)" (to-string (get-rid-of-implication (disjunction (negation (conjunction (variable :a) (variable :b))) (implication (variable :c) (variable :d)))))))
  )
)

(deftest push-negation-test
  (testing "Проталкивание отрицания до литералов"
    (is (= "1" (to-string (push-negation (constant 1)))))
    (is (= "x" (to-string (push-negation (variable :x)))))
    (is (= "x" (to-string (push-negation (negation (negation (variable :x)))))))
    (is (= "~x|0" (to-string (push-negation (negation (conjunction (variable :x) (constant 1)))))))
    (is (= "~x&0" (to-string (push-negation (negation (disjunction (variable :x) (constant 1)))))))
    (is (= "x&0" (to-string (push-negation (negation (disjunction (negation (variable :x)) (constant 1)))))))
    (is (= "x&(x&1)" (to-string (push-negation (negation (disjunction (negation (variable :x)) (negation (conjunction (variable :x) (constant 1)))))))))
  )
)

(deftest distribute-test
  (testing "Дистрибутивность"
    (is (= "1" (to-string (distribute (constant 1)))))
    (is (= "x" (to-string (distribute (variable :x)))))
    (is (= "~1" (to-string (distribute (negation (constant 1))))))
    (is (= "~x" (to-string (distribute (negation (variable :x))))))
    (is (= "x&1" (to-string (distribute (conjunction (variable :x) (constant 1))))))
    (is (= "x|1" (to-string (distribute (disjunction (variable :x) (constant 1))))))
    (is (= "(x&y)|1" (to-string (distribute (disjunction (conjunction (variable :x) (variable :y)) (constant 1))))))
    (is (= "(1&x)|(1&y)" (to-string (distribute (conjunction (constant 1) (disjunction (variable :x) (variable :y)))))))
    (is (= "(x&1)|(y&1)" (to-string (distribute (conjunction (disjunction (variable :x) (variable :y)) (constant 1))))))
    (is (= "(a&(b&c))|(a&(b&d))" (to-string (distribute (conjunction (variable :a) (conjunction (variable :b) (disjunction (variable :c) (variable :d))))))))
    (is (= "((a&c)&d)|((b&c)&d)" (to-string (distribute (conjunction (conjunction (disjunction (variable :a) (variable :b)) (variable :c)) (variable :d))))))
  )
)

(deftest associate-test
  (testing "Ассоциативность конъюнкции и дизъюнкции"
    (is (= "a&b&c" (to-string (associate (conjunction (variable :a) (conjunction (variable :b) (variable :c)))))))
    (is (= "0&a&b&c" (to-string (associate (conjunction (conjunction (constant 0) (variable :a)) (conjunction (variable :b) (variable :c)))))))
    (is (= "a|b|c" (to-string (associate (disjunction (variable :a) (disjunction (variable :b) (variable :c)))))))
    (is (= "0|a|b|c" (to-string (associate (disjunction (disjunction (constant 0) (variable :a)) (disjunction (variable :b) (variable :c)))))))
    (is (= "0|a|(b&c&d)" (to-string (associate (disjunction (disjunction (constant 0) (variable :a)) (conjunction (variable :b) (conjunction (variable :c) (variable :d))))))))
  )
)

(deftest get-rid-of-constants-test
  (testing "Избавление от констант"
    (is (= "0" (to-string (get-rid-of-constants (constant 0)))))
    (is (= "x" (to-string (get-rid-of-constants (variable :x)))))
    (is (= "0" (to-string (get-rid-of-constants (conjunction (variable :x) (constant 0))))))
    (is (= "x" (to-string (get-rid-of-constants (conjunction (variable :x) (constant 1))))))
    (is (= "0" (to-string (get-rid-of-constants (conjunction (variable :x) (variable :y) (constant 0))))))
    (is (= "x&y" (to-string (get-rid-of-constants (conjunction (variable :x) (variable :y) (constant 1))))))
    (is (= "x" (to-string (get-rid-of-constants (disjunction (variable :x) (constant 0))))))
    (is (= "1" (to-string (get-rid-of-constants (disjunction (variable :x) (constant 1))))))
    (is (= "x|y" (to-string (get-rid-of-constants (disjunction (variable :x) (variable :y) (constant 0))))))
    (is (= "1" (to-string (get-rid-of-constants (disjunction (variable :x) (variable :y) (constant 1))))))
    (is (= "~0" (to-string (get-rid-of-constants (negation (constant 0))))))
    (is (= "~x" (to-string (get-rid-of-constants (negation (variable :x))))))
    (is (= "~0" (to-string (get-rid-of-constants (negation (conjunction (variable :x) (constant 0)))))))
    (is (= "~x" (to-string (get-rid-of-constants (negation (conjunction (variable :x) (constant 1)))))))
    (is (= "~0" (to-string (get-rid-of-constants (negation (conjunction (variable :x) (variable :y) (constant 0)))))))
    (is (= "~(x&y)" (to-string (get-rid-of-constants (negation (conjunction (variable :x) (variable :y) (constant 1)))))))
    (is (= "~x" (to-string (get-rid-of-constants (negation (disjunction (variable :x) (constant 0)))))))
    (is (= "~1" (to-string (get-rid-of-constants (negation (disjunction (variable :x) (constant 1)))))))
    (is (= "~(x|y)" (to-string (get-rid-of-constants (negation (disjunction (variable :x) (variable :y) (constant 0)))))))
    (is (= "~1" (to-string (get-rid-of-constants (negation (disjunction (variable :x) (variable :y) (constant 1)))))))
  )
)

(deftest dnf-test
  (testing "Приведение к ДНФ"
    (is (= "0" (to-string (dnf (constant 0)))))
    (is (= "x" (to-string (dnf (variable :x)))))
    (is (= "1" (to-string (dnf (negation (constant 0))))))
    (is (= "~x" (to-string (dnf (negation (variable :x))))))
    (is (= "0" (to-string (dnf (conjunction (variable :x) (constant 0))))))
    (is (= "x" (to-string (dnf (conjunction (variable :x) (constant 1))))))
    (is (= "x&y" (to-string (dnf (conjunction (variable :x) (conjunction (variable :y) (constant 1)))))))
    (is (= "x" (to-string (dnf (disjunction (variable :x) (constant 0))))))
    (is (= "1" (to-string (dnf (disjunction (variable :x) (constant 1))))))
    (is (= "x|y" (to-string (dnf (disjunction (variable :x) (disjunction (variable :y) (constant 0)))))))
    (is (= "~x|y" (to-string (dnf (implication (variable :x) (variable :y))))))
    (is (= "1" (to-string (dnf (implication (constant 0) (conjunction (variable :y) (variable :z)))))))
    (is (= "0" (to-string (dnf (implication (constant 1) (constant 0))))))
    (is (= "a&b" (to-string (dnf (conjunction (conjunction (constant 1) (variable :a)) (conjunction (constant 1) (variable :b)))))))
  )
)

(deftest apply-values-test
  (testing "Подстановка значений переменных"
    (def expr (implication (negation (conjunction (variable :a) (variable :b))) (disjunction (variable :c) (negation (variable :d)))))
    (is (= "~(0&b)->(c|~d)" (to-string (apply-values expr (hash-map (variable :a) (constant 0))))))
    (is (= "~(x&b)->(c|~d)" (to-string (apply-values expr (hash-map (variable :a) (variable :x))))))
    (is (= "(0&b)|c|~d" (to-string (dnf (apply-values expr (hash-map (variable :a) (constant 0)))))))
    (is (= "~(1&b)->(c|~d)" (to-string (apply-values expr (hash-map (variable :a) (constant 1))))))
    (is (= "(1&b)|c|~d" (to-string (dnf (apply-values expr (hash-map (variable :a) (constant 1)))))))
    (is (= "~(a&0)->(c|~d)" (to-string (apply-values expr (hash-map (variable :b) (constant 0))))))
    (is (= "(a&0)|c|~d" (to-string (dnf (apply-values expr (hash-map (variable :b) (constant 0)))))))
    (is (= "~(a&1)->(c|~d)" (to-string (apply-values expr (hash-map (variable :b) (constant 1))))))
    (is (= "~(a&b)->(0|~d)" (to-string (apply-values expr (hash-map (variable :c) (constant 0))))))
    (is (= "~(a&b)->(1|~d)" (to-string (apply-values expr (hash-map (variable :c) (constant 1))))))
    (is (= "~(a&b)->(c|~0)" (to-string (apply-values expr (hash-map (variable :d) (constant 0))))))
    (is (= "~(a&b)->(c|~1)" (to-string (apply-values expr (hash-map (variable :d) (constant 1))))))
    (is (= "~(0&1)->(0|~1)" (to-string (apply-values expr (hash-map (variable :a) (constant 0) (variable :b) (constant 1) (variable :c) (constant 0) (variable :d) (constant 1))))))
  )
)
