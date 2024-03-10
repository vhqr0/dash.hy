(require
  dash.dash *)

(import
  dash.dash *
  dash.strtools :as s
  functools [singledispatch])

;; applicative let
(defmacro alet [bindings #* body]
  `(let ~bindings
     ~(let [names (list (-take-nth 2 bindings))]
        (--reduce-from
          `(.apply ~acc ~it)
          `(.wrap ~(first names)
                  ~(--reduce-from
                     `(fn [~it] ~acc)
                     `(do ~@body)
                     (reversed names)))
          names))))

;; monad let
(defmacro mlet [bindings #* body]
  (->> (-partition-all 2 bindings)
       list
       reversed
       (--reduce-from
         (let [#(l r) it]
           (cond (= l :setv) `(let ~r ~acc)
                 (symbol? l) `(.bind ~r (fn [~l] ~acc))
                 True `(.bind ~r (fn [it] (let [~l it] ~acc)))))
         `(do ~@body))))



(defclass monad []
  ;; applicative
  (defn __mul__ [self mv]
    (.apply self mv))

  ;; monad
  (defn __or__ [self fm]
    (.bind self fm))

  (defn __str__ [self]
    (s.format "<{}>" self.__class__.__name__))

  (defn __repr__ [self]
    (str self))

  (defn [classmethod] wrap [cls [v None]]
    (raise NotImplementedError))

  (defn [classmethod] zero [cls]
    (cls.wrap))

  (defn unwrap [cls]
    (raise NotImplementedError))

  ;; functor
  (defn map [self f]
    (raise NotImplementedError))

  (defn update [self f #* args #** kwargs]
    (.map self (fn [it] (f it #* args #** kwargs))))

  ;; applicative
  (defn apply [self mv]
    (raise NotImplementedError))

  ;; monad
  (defn bind [self fm]
    (raise NotImplementedError)))

(defn monad? [o] (isinstance o monad))



(defclass zero-monad-mixin []
  (defn __bool__ [self] False)
  (defn unwrap [self])
  (defn map [self f] self)
  (defn apply [self mv] self)
  (defn bind [self fm] self))

(defclass value-monad-mixin []
  (defn __init__ [self [v None]]
    (setv self.v v))

  (defn __str__ [self]
    (s.format "<{} {}>" self.__class__.__name__ (repr self.v)))

  (defn unwrap [self]
    self.v))

(defclass zero-value-monad-mixin [value-monad-mixin zero-monad-mixin])

(defclass nonzero-value-monad-mixin [value-monad-mixin]
  (defn map [self f]
    (self.wrap (f self.v)))

  (defn apply [self mv]
    (if mv (-> (.unwrap mv) (self.v) (self.wrap)) mv))

  (defn bind [self fm]
    (fm self.v)))

(defclass fn-value-monad-mixin [value-monad-mixin]
  (defn [classmethod] wrap-fn [cls f]
    (cls f))

  (defn run [self #* args #** kwargs]
    (self.v #* args #** kwargs)))



(defclass mid [nonzero-value-monad-mixin monad]
  (defn [classmethod] wrap [cls [v None]] (cls v)))

(defclass maybe [monad]
  (defn [classmethod] wrap [cls [v None]]
    (if (none? v) (nothing) (just v))))

(defclass just [nonzero-value-monad-mixin maybe])
(defclass nothing [zero-monad-mixin maybe])

(defclass either [value-monad-mixin monad]
  (defn [classmethod] wrap [cls [v None]] (right v))
  (defn [classmethod] zero [cls] (left)))

(defclass right [nonzero-value-monad-mixin either])
(defclass left [zero-value-monad-mixin either])



(defmacro mtry! [#* body]
  `(mtry.wrap-fn (fn [] ~@body)))

(defclass mtry [value-monad-mixin monad]
  (defn [classmethod] wrap-fn [cls f]
    (try
      (success (f))
      (except [e Exception]
        (failure e))))

  (defn [classmethod] wrap [cls [v None]] (success v))
  (defn [classmethod] zero [cls] (failure (Exception))))

(defclass success [nonzero-value-monad-mixin mtry])

(defclass failure [zero-value-monad-mixin mtry]
  (defn unwrap [self] (raise self.v)))



(defmacro state! [#* body]
  `(state (fn [s] ~@body)))

;; s->(v,s)
(defclass state [fn-value-monad-mixin monad]
  (defn [classmethod] wrap [cls [v None]]
    (cls.wrap-fn (fn [s] #(v s))))

  ;; mt :: s->(t,s)
  ;; self :: s->(a,s) :: ma
  ;; fm :: a->(s->(b,s)) :: a->mb
  ;; self->fm->(s->(b,s)) :: self->fm->mb
  (defn bind [self fm]
    (self.wrap-fn
      (fn [s]
        (let [#(v ns) (.run self s)]
          (.run (fm v) ns))))))

(defmacro reader! [#* body]
  `(reader (fn [e] ~@body)))

;; e->v
(defclass reader [fn-value-monad-mixin monad]
  (defn [classmethod] wrap [cls [v None]]
    (cls.wrap-fn (fn [e] v)))

  ;; mt :: e->t
  ;; self :: e->a :: ma
  ;; fm :: a->(e->b) :: a->mb
  ;; self->fm->(e->b) :: self->fm->mb
  (defn bind [self fm]
    (self.wrap-fn
      (fn [e]
        (.run (fm (.run self e)) e)))))



(defmacro cont! [#* body]
  `(cont (fn [k] ~@body)))

;; (v->r)->r
(defclass cont [fn-value-monad-mixin monad]
  (defn [classmethod] wrap [cls [v None]]
    (cls (fn [k] (k v))))

  ;; mt :: (t->r)->r
  ;; self :: (a->r)->r :: ma
  ;; fm :: a->((b->r)->r) :: a->mb
  ;; self->fm->((b->r)->r) :: self->fm->mb
  (defn bind [self fm]
    (self.wrap-fn
      (fn [k]
        (.run self (fn [v] (.run (fm v) k))))))

  ;; mt :: (t->r)->r
  ;; cc :: (a->((b->r)->r))->((a->r)->r) :: (a->mb)->ma
  ;; cc->((a->r)->r) :: cc->ma
  (defn [classmethod] call-cc [cls cc]
    (cls.wrap-fn
      (fn [k]
        ;; v :: a
        ;; k :: a->r
        ;; exit :: a->((b->r)->r) :: a->mb
        (let [exit (fn [v] (cls.wrap-fn (fn [_] (k v))))]
          ;; cc :: (a->mb)->ma
          ;; exit :: a->mb
          ;; (cc exit) :: (a->r)->r :: ma
          ;; k :: a->r
          (.run (cc exit) k))))))

(defmacro call-cc [exit #* body]
  `(cont.call-cc (fn [~exit] ~@body)))



(export
  :objects [monad monad? mid maybe just nothing either right left mtry success failure
            state reader cont]
  :macros [alet mlet mtry! state! reader! cont! call-cc])
