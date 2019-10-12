(ns lab4.generator-test
  (:require [clojure.test :refer :all]
            [lab4.generator :refer :all]))

(deftest last_symbol_is_not_test
  (testing "Последний символ не равен"
    (is (not (last_symbol_is_not "alexander" \r)))
    (is (last_symbol_is_not "alexander" \a))
  )
)

(deftest append_symbol_to_word_test
  (testing "Добавить символ в конец слова"
    (is (= "alexander" (append_symbol_to_word "alexande" \r)))
  )
)

(deftest append_symbols_to_word_test
  (testing "К каждому символу добавить в качестве префикса слово"
    (is
      (=
        (list "alex-1" "alex-2" "alex-3")
        (append_symbols_to_word "alex-" (list \1 \2 \3))
      )
    )
  )
)

(deftest append_symbols_to_word_test
  (testing "Не добавлять в конец слова повторяющийся символ"
    (is
      (=
        (list "alexanders" "alexanderf")
        (append_symbols_to_word "alexander" (list \s \f \r))
      )
    )
  )
)

(deftest list_flatten_test
  (testing "Сделать список плоским"
    (is
      (=
        (list 1 2 (list 3) 4)
        (list_flatten (list (list 1 2) (list (list 3) 4)))
      )
    )
  )
)

(deftest append_symbols_to_words_test
  (testing "Добавить каждый символ в конец каждого списка и расплющить получившийся список"
    (is
      (=
        (list "abcdi" "abcdj" "abcdk" "efghi" "efghj" "efghk")
        (append_symbols_to_words (list "abcd" "efgh") (list \i \j \k))
      )
    )
  )
)

(deftest generate_symbol_sequence_test
  (testing ""
    (is (= (list) (generate_symbol_sequence (list \a \b) -1)))
    (is (= (list) (generate_symbol_sequence (list \a \b) 0)))
    (is (= (list "a" "b") (generate_symbol_sequence (list \a \b) 1)))
    (is (= (list "ab" "ba") (generate_symbol_sequence (list \a \b) 2)))
    (is (= (list "aba" "bab") (generate_symbol_sequence (list \a \b) 3)))
    (is (= (list "aba" "bab") (generate_symbol_sequence (list \a \b \a \b) 3)))
  )
)
