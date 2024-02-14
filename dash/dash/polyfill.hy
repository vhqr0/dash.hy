(import
  collections.abc [Callable Iterator Iterable Hashable Sized Reversible Sequence Set Mapping]
  types [ModuleType FunctionType MethodType])

(setv symbol   hy.models.Symbol
      keyword  hy.models.Keyword
      sexp     hy.models.Expression)

(defn ignore [#* args #** kwargs])

(defn identity    [o] o)
(defn constantly  [o] (fn [] o))
(defn none?       [o] (is o None))
(defn true?       [o] (is o True))
(defn false?      [o] (is o False))
(defn bool?       [o] (isinstance o bool))
(defn type?       [o] (isinstance o type))
(defn module?     [o] (isinstance o ModuleType))
(defn fn?         [o] (isinstance o FunctionType))
(defn method?     [o] (isinstance o MethodType))
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
(defn memoryview? [o] (isinstance o memoryview))
(defn slice?      [o] (isinstance o slice))
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



(defmacro comment [#* body])

(defmacro ignore [#* body]
  `(do ~@body None))

(defmacro unless [test #* body]
  `(when (not ~test) ~@body))

(defmacro if-let [binding then else]
  (let [#(name test) binding]
    `(let [~name ~test] (if ~name ~then ~else))))

(defmacro when-let [binding #* body]
  (let [#(name test) binding]
    `(let [~name ~test] (when ~name ~@body))))

(defmacro loop [bindings form]
  (assert (even? (len bindings)))
  (let [names (lfor #(i it) (enumerate bindings) :if (even? i) it)]
    (defn replace-recur [form]
      (if (sexp? form)
          (if (= 'recur (get form 0))
              (if names
                  `(do
                     (setv #(~@names) #(~@(cut form 1 None)))
                     (continue))
                  '(continue))
              (sexp (map replace-recur form)))
          form))
    (if names
        `((fn []
            (let [~@bindings]
              (while True
                (return ~(replace-recur form))))))
        `((fn []
            (while True
              (return ~(replace-recur form))))))))



(export
  :objects [ignore identity constantly none? true? false? bool?
            type? module? fn? method? callable?
            iter? iterable? hashable? countable? reversible? sequence? set? map?
            symbol? keyword? sexp? symbol keyword sexp
            str? bytes? bytearray? memoryview? slice? int? float? number?
            zero? pos? neg? even? odd? inc dec]
  :macros [comment ignore unless if-let when-let loop])
