(eval-and-compile
  (import hy.pyops *))


;;; simple

(defmacro comment [#* body]
  `(do))

(defmacro ignore [#* body]
  `(do ~@body None))

(eval-and-compile
  (defn ignore [#* args #** kwargs]))

;; if when unless

(defmacro unless [test #* body]
  `(when (not ~test) ~@body))

(defmacro ap-if [test then else]
  `(let [it ~test]
     (if it ~then ~else)))

(defmacro ap-when [test #* body]
  `(let [it ~test]
     (when it ~@body)))

(defmacro ap-unless [test #* body]
  `(let [it ~test]
     (unless it ~@body)))

;; doiter dotimes

(defmacro ap-doiter [iterable form]
  `(for [it ~iterable] ~form))

(defmacro ap-dotimes [n form]
  `(ap-doiter (range ~n) ~form))

(eval-and-compile
  (defn doiter [iterable f]
    (ap-doiter iterable (f it)))
  (defn dotimes [n f]
    (ap-dotimes n (f it))))

;; reduce map filter remove

(defmacro make-transducer [#* body]
  (import itertools [batched])
  (let [meta (dict (batched body 2))]
    `(fn [rf]
       (fn [#* args]
         ~@(ap-when (.get meta ':nonlocals)
                    `((nonlocal ~@it)))
         (match args
                #(acc) ~(.get meta ':comp '(rf acc))
                #(acc it) ~(.get meta ':step '(rf acc it))
                _ (raise IndexError))))))

(eval-and-compile
  (import
    functools [reduce :as _reduce]
    builtins [map :as _map]
    builtins [filter :as _filter]
    itertools [filterfalse :as _remove]))

(eval-and-compile
  (defn reduce [#* args]
    (match args
           #(f iterable) (_reduce f iterable)
           #(f init iterable) (_reduce f iterable init)
           _ (raise IndexError))))

(eval-and-compile
  (defn map [#* args]
    (match args
           #(f) (make-transducer
                  :step (rf acc (f it)))
           #(f #* iterables) (_map f #* iterables))))

(eval-and-compile
  (defn filter [#* args]
    (match args
           #(pred) (make-transducer
                     :step (if (pred it) (rf acc it) acc))
           #(pred iterable) (_filter pred iterable)
           _ (raise IndexError))))

(eval-and-compile
  (defn remove [#* args]
    (match args
           #(pred) (make-transducer
                     :step (if (pred it) acc (rf acc it)))
           #(pred iterable) (_remove pred iterable)
           _ (raise IndexError))))

(defmacro ap-reduce [#* args]
  (match args
         #(form iterable) `(reduce (fn [acc it] ~form) ~iterable)
         #(form init iterable) `(reduce (fn [acc it] ~form) ~init ~iterable)
         _ (raise IndexError)))

(defmacro ap-map [#* args]
  (match args
         #(form) `(map (fn [it] ~form))
         #(form iterable) `(map (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defmacro ap-filter [#* args]
  (match args
         #(form) `(filter (fn [it] ~form))
         #(form iterable) `(filter (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defmacro ap-remove [#* args]
  (match args
         #(form) `(remove (fn [it] ~form))
         #(form iterable) `(remove (fn [it] ~form) ~iterable)
         _ (raise IndexError)))


;;; types

(eval-and-compile
  (import
    collections.abc [Callable Iterator Generator Iterable Hashable Sized :as Countable Reversible
                     Sequence Mapping Set MutableSequence MutableMapping MutableSet]
    types [ModuleType FunctionType MethodType])
  (defn none?       [x] (is x None))
  (defn true?       [x] (is x True))
  (defn false?      [x] (is x False))
  (defn bool?       [x] (isinstance x bool))
  (defn type?       [x] (isinstance x type))
  (defn mod?        [x] (isinstance x ModuleType))
  (defn fn?         [x] (isinstance x FunctionType))
  (defn method?     [x] (isinstance x MethodType))
  (defn callable?   [x] (isinstance x Callable))
  (defn exception?  [x] (isinstance x Exception))
  (defn iter?       [x] (isinstance x Iterator))
  (defn generator?  [x] (isinstance x Generator))
  (defn iterable?   [x] (isinstance x Iterable))
  (defn hashable?   [x] (isinstance x Hashable))
  (defn countable?  [x] (isinstance x Countable))
  (defn reversible? [x] (isinstance x Reversible))
  (defn sequence?   [x] (isinstance x Sequence))
  (defn mapping?    [x] (isinstance x Mapping))
  (defn set?        [x] (isinstance x Set))
  (defn mutableseq? [x] (isinstance x MutableSequence))
  (defn mutablemap? [x] (isinstance x MutableMapping))
  (defn mutableset? [x] (isinstance x MutableSet))
  (defn coll?       [x] (and (iterable? x) (not (isinstance x #(str bytes)))))
  (defn tuple?      [x] (isinstance x tuple))
  (defn list?       [x] (isinstance x list))
  (defn dict?       [x] (isinstance x dict))
  (defn str?        [x] (isinstance x str))
  (defn bytes?      [x] (isinstance x bytes))
  (defn bytearray?  [x] (isinstance x bytearray))
  (defn memoryview? [x] (isinstance x memoryview))
  (defn slice?      [x] (isinstance x slice))
  (defn int?        [x] (isinstance x int))
  (defn float?      [x] (isinstance x float))
  (defn complex?    [x] (isinstance x complex))
  (defn number?     [x] (isinstance x #(int float complex))))


;;; inttools

(eval-and-compile
  (defn zero? [i] (= i 0))
  (defn pos?  [i] (> i 0))
  (defn neg?  [i] (< i 0))
  (defn even? [i] (zero? (& i 1)))
  (defn odd?  [i] (not (even? i)))
  (defn inc   [i] (+ i 1))
  (defn dec   [i] (- i 1)))


;;; listtools

(eval-and-compile
  (defn list-into! [l iterable] (.extend l iterable) l)
  (defn list-conj! [l x] (.append l x) l))


;;; hymodels

(eval-and-compile
  (setv model   hy.models.Object
        keyword hy.models.Keyword
        symbol  hy.models.Symbol
        sexp    hy.models.Expression
        hytuple hy.models.Tuple
        hylist  hy.models.List
        hydict  hy.models.Dict
        hyset   hy.models.Set
        hystr   hy.models.String
        hybytes hy.models.Bytes)
  (defn model?   [x] (isinstance x model))
  (defn keyword? [x] (isinstance x keyword))
  (defn symbol?  [x] (isinstance x symbol))
  (defn sexp?    [x] (isinstance x sexp))
  (defn hytuple? [x] (isinstance x hytuple))
  (defn hylist?  [x] (isinstance x hylist))
  (defn hyseq?   [x] (isinstance x #(hytuple hylist)))
  (defn hydict?  [x] (isinstance x hydict))
  (defn hyset?   [x] (isinstance x hyset))
  (defn hystr?   [x] (isinstance x hystr))
  (defn hybytes? [x] (isinstance x hybytes)))


;;; functools

(eval-and-compile
  (defn identity [o] o)
  (defn constantly [o] (fn [] o)))

(eval-and-compile
  (import functools [partial])
  (defn curry [#* args]
    (match args
           #() curry
           #(n) (partial curry n)
           #(n f #* args) (if (>= (len args) n)
                              (f #* args)
                              (partial curry n f #* args)))))

(eval-and-compile
  (defn comp [#* fns]
    (match fns
           #() identity
           #(f) f
           #(f g #* fns) (comp (fn [x] (f (g x))) #* fns))))

(eval-and-compile
  (defn juxt [#* fns]
    (fn [#* args #** kwargs]
      (tuple (ap-map (it #* args #** kwargs) fns)))))

(eval-and-compile
  (defn trampoline [x]
    (while (fn? x) (setv x (x)))
    x))

(eval-and-compile
  (defn notfn [pred]
    (fn [x] (not (pred x))))
  (defn orfn [#* preds]
    (fn [x] (any (ap-map (it x) preds))))
  (defn andfn [#* preds]
    (fn [x] (all (ap-map (it x) preds))))
  (setv complement notfn))

(eval-and-compile
  (defn completing [f [cf identity]]
    (fn [#* args]
      (match args
             #() (f)
             #(acc) (cf acc)
             #(acc it) (f acc it)
             _ (raise IndexError)))))


;;; threading macros

(defmacro -> [x #* forms]
  (match forms
         #() x
         #(form #* forms) `(-> ~(match form
                                       #(y #* xs) `(~y ~x ~@xs)
                                       _ `(~form ~x))
                               ~@forms)))

(defmacro ->> [x #* forms]
  (match forms
         #() x
         #(form #* forms) `(->> ~(match form
                                        #(#* xs) `(~@xs ~x)
                                        _ `(~form ~x))
                                ~@forms)))

(defmacro as-> [x sym #* forms]
  (match forms
         #() x
         #(form #* forms) `(as-> (let [~sym ~x] ~form)
                                 ~sym ~@forms)))

(defmacro doto [x #* forms]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(ap-map `(-> $ ~it) forms)
       ~$)))


;;; let macros

(defmacro if-let [bindings then else]
  (let [$ (hy.gensym)]
    (match bindings
           #() then
           #(l r #* bindings) (cond (symbol? l)
                                    (do
                                      `(let [~l ~r]
                                         (if ~l
                                             (if-let [~@bindings]
                                               ~then ~else)
                                             ~else)))
                                    (hyseq? l)
                                    (do
                                      `(let [~$ ~r]
                                         (if ~$
                                             (let [~l ~$]
                                               (if-let [~@bindings]
                                                 ~then ~else))
                                             ~else)))
                                    True
                                    (raise TypeError))
           _ (raise IndexError))))

(defmacro when-let [bindings #* body]
  (let [$ (hy.gensym)]
    (match bindings
           #() `(do ~@body)
           #(l r #* bindings) (cond (symbol? l)
                                    (do
                                      `(let [~l ~r]
                                         (when ~l
                                           (when-let [~@bindings]
                                             ~@body))))
                                    (hyseq? l)
                                    (do
                                      `(let [~$ ~r]
                                         (when ~$
                                           (let [~l ~$]
                                             (when-let [~@bindings]
                                               ~@body)))))
                                    True
                                    (raise TypeError))
           _ (raise IndexError))))


;;; loop

(defmacro loop [#* args]
  (import itertools [batched])
  (let [#(async? bindings form) (match args
                                       #(:async bindings form) #(True bindings form)
                                       #(bindings form) #(False bindings form)
                                       _ (raise IndexError))
        ls (->> (batched bindings 2) (ap-map (let [#(l r) it] l)) list)]
    (assert (all (map symbol? ls)))
    (defn recur-replace [form]
      (if (sexp? form)
          (let [#(x #* xs) form]
            (if (= x 'recur)
                `(do
                   ~@(when ls `((setv #(~@ls) #(~@xs))))
                   (continue))
                (sexp (map recur-replace form))))
          form))
    (let [form `(while True (return ~(recur-replace form)))]
      (when bindings
        (setv form `(let ~bindings ~form)))
      (if async?
          `(await ((fn/a [] ~form)))
          `((fn [] ~form))))))
