(import
  collections.abc [Callable Iterator Iterable Hashable Sized Reversible Sequence Set Mapping])

(setv function (type (fn []))
      symbol   hy.models.Symbol
      keyword  hy.models.Keyword
      sexp     hy.models.Expression)

(defn ignore [#* args #** kwargs])

(defn identity    [o] o)
(defn none?       [o] (is o None))
(defn true?       [o] (is o True))
(defn false?      [o] (is o False))
(defn bool?       [o] (isinstance o bool))
(defn type?       [o] (isinstance o type))
(defn fn?         [o] (isinstance o function))
(defn callable?   [o] (isinstance o Callable))
(defn iter?       [o] (isinstance o Iterator))
(defn iterable?   [o] (isinstance o Iterable))
(defn hashable?   [o] (isinstance o Hashable))
(defn countable?  [o] (isinstance o Sized))
(defn reversible? [o] (isinstance o Reversible))
(defn sequence?   [o] (isinstance o Sequence))
(defn set?        [o] (isinstance o Set))
(defn map?        [o] (isinstance o Mapping))
(defn symbol?     [o] (isinstance o symbol))
(defn keyword?    [o] (isinstance o keyword))
(defn sexp?       [o] (isinstance o sexp))
(defn str?        [o] (isinstance o str))
(defn bytes?      [o] (isinstance o bytes))
(defn bytearray?  [o] (isinstance o bytearray))
(defn int?        [o] (isinstance o int))
(defn float?      [o] (isinstance o float))
(defn number?     [o] (isinstance o #(int float)))
(defn zero?       [i] (= i 0))
(defn pos?        [i] (> i 0))
(defn neg?        [i] (< i 0))
(defn even?       [i] (zero? (& i 1)))
(defn odd?        [i] (not (even? i)))
(defn inc         [i] (+ i 1))
(defn dec         [i] (- i 1))



(defmacro loop [bindings form]
  (let [names (lfor #(i it) (enumerate bindings) :if (even? i) it)]
    (defn replace-recur [form]
      (if (sexp? form)
          (if (= (get form 0) 'recur)
              `(do
                 (setv #(~@names) #(~@(cut form 1 None)))
                 (continue))
              (sexp (map replace-recur form)))
          form))
    `((fn []
        (let [~@bindings]
          (while True
            (return ~(replace-recur form))))))))

(defmacro unless [test #* body] `(when (not ~test) ~@body))



(export
  :objects [ignore identity none? true? false? bool?
            type? fn? callable? function
            iter? iterable? hashable? countable? reversible? sequence? set? map?
            symbol? keyword? sexp? symbol keyword sexp
            str? bytes? bytearray? int? float? number?
            zero? pos? neg? even? odd? inc dec]
  :macros [loop unless])
