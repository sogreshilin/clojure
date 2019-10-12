(ns lab4.generator)

(defn last_symbol_is_not
  "Проверить, равен ли последний символ слова word символу symbol"
  [word symbol]
  (
    not= (last word) symbol
  )
)

(defn append_symbol_to_word
  "Добавить символ symbol в конец слова word"
  [word symbol]
  (
    str word symbol
  )
)

(defn append_symbols_to_word
  "К каждому символу, кроме тех, которые совпадают с последним символом слова word,
   добавить в начало префикс word"
  [word symbols]
  (
    map (fn [symbol] (str word symbol)) (filter (fn [symbol] (last_symbol_is_not word symbol)) symbols)
  )
)

(defn list_flatten
  "Сделать список списков плоским"
  [a_list]
  (
    reduce concat (list) a_list
  )
)

(defn append_symbols_to_words
  "К каждому слову из списка words добавить по одному символу из списка symbols,
   исключая ситуации, когда последняя буква слова совпадает с добавляемым символом"
  [words symbols]
  (
    list_flatten (map (fn [word] (append_symbols_to_word word symbols)) words)
  )
)

(defn generate_symbol_sequence
  "Функция генерирует список строк длины length из символов symbols не содержащих двух подряд идущих одинаковых символов"
  [symbols length]
  (let [distinct_symbols (distinct symbols)]
    (cond
      (<= length 0) (list)
      :default (
        reduce (fn [acc iteration] (append_symbols_to_words acc distinct_symbols)) (map str distinct_symbols) (range (- length 1))
      )
    )
  )
)
