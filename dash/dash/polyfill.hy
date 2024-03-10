(import
  collections.abc [Callable Iterator Generator Iterable Hashable Sized Reversible Sequence Mapping Set]
  types [ModuleType FunctionType MethodType])

(setv model      hy.models.Object
      keyword    hy.models.Keyword
      symbol     hy.models.Symbol
      sexp       hy.models.Expression
      tuplemodel hy.models.Tuple
      listmodel  hy.models.List
      dictmodel  hy.models.Dict
      setmodel   hy.models.Set
      strmodel   hy.models.String
      bytesmodel hy.models.Bytes)

(defn ignore [#* args #** kwargs])
(defn with-ignore [v #* args #** kwargs] v)

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
(defn exception?  [o] (isinstance o Exception))
(defn iter?       [o] (isinstance o Iterator))
(defn generator?  [o] (isinstance o Generator))
(defn iterable?   [o] (isinstance o Iterable))
(defn coll?       [o] (and (iterable? o) (not (isinstance o #(str bytes)))))
(defn hashable?   [o] (isinstance o Hashable))
(defn countable?  [o] (isinstance o Sized))
(defn reversible? [o] (isinstance o Reversible))
(defn sequence?   [o] (isinstance o Sequence))
(defn map?        [o] (isinstance o Mapping))
(defn set?        [o] (isinstance o Set))
(defn tuple?      [o] (isinstance o tuple))
(defn list?       [o] (isinstance o list))
(defn dict?       [o] (isinstance o dict))
(defn model?      [o] (isinstance o model))
(defn keyword?    [o] (isinstance o keyword))
(defn symbol?     [o] (isinstance o symbol))
(defn sexp?       [o] (isinstance o sexp))
(defn tuplemodel? [o] (isinstance o tuplemodel))
(defn listmodel?  [o] (isinstance o listmodel))
(defn dictmodel?  [o] (isinstance o dictmodel))
(defn setmodel?   [o] (isinstance o setmodel))
(defn strmodel?   [o] (isinstance o strmodel))
(defn bytesmodel? [o] (isinstance o bytesmodel))
(defn str?        [o] (isinstance o str))
(defn bytes?      [o] (isinstance o bytes))
(defn bytearray?  [o] (isinstance o bytearray))
(defn memoryview? [o] (isinstance o memoryview))
(defn slice?      [o] (isinstance o slice))
(defn int?        [o] (isinstance o int))
(defn float?      [o] (isinstance o float))
(defn complex?    [o] (isinstance o complex))
(defn number?     [o] (isinstance o #(int float complex)))
(defn zero?       [i] (= i 0))
(defn pos?        [i] (> i 0))
(defn neg?        [i] (< i 0))
(defn even?       [i] (zero? (& i 1)))
(defn odd?        [i] (not (even? i)))
(defn inc         [i] (+ i 1))
(defn dec         [i] (- i 1))



(defmacro comment [#* body]
  '(do))

(defmacro ignore [#* body]
  `(do ~@body None))

(defmacro with-ignore [v #* body]
  `(do ~@body ~v))

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
  :objects [model keyword symbol sexp tuplemodel listmodel dictmodel setmodel strmodel bytesmodel
            ignore with-ignore identity constantly none? true? false? bool? type? module? fn? method? callable? exception?
            iter? generator? iterable? coll? hashable? countable? reversible? sequence? map? set? tuple? list? dict?
            model? keyword? symbol? sexp? tuplemodel? listmodel? dictmodel? setmodel? strmodel? bytesmodel?
            str? bytes? bytearray? memoryview? slice? int? float? complex? number?
            zero? pos? neg? even? odd? inc dec]
  :macros [comment ignore with-ignore unless if-let when-let loop])
