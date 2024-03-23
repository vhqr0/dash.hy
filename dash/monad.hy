(require
  dash.core *)

(import
  dash.core *
  dash.metaclasses [singleton-meta]
  dataclasses [dataclass]
  typing [Any])

(defmacro alet [bindings #* body]
  #[["Applicative functor let."
     ["1. Parallel computing and binding functors `fvs'"
      "2. Wrap body to a curried applicative functor `af'"
      "3. Apply functors `fvs' to Applicative functor `af'"]

     ::example
     (alet [a (just 1)
            b (just 2)
            c (just 3)]
       (+ a b c))
     ;; just(6)
     ;; macroexpand
     (let [a (just 1)
           b (just 2)
           c (just 3)]
       (-> (fn [a] (fn [b] (fn [c] (+ a b c))))
           (monad.fapply a b c)))
     ]]
  (let [ls (list (take-nth 2 bindings))]
    (assert (all? symbol? ls))
    `(let ~bindings
       (-> ~(ap-reduce `(fn [~it] ~acc) `(do ~@body) (reversed ls))
           (monad.fapply ~@ls)))))

(defmacro mlet [bindings #* body]
  #[["Monad let."

     ::example
     (mlet [a (just 1)
            b (just (inc a))
            c (just (inc b))]
       (just (+ a b c)))
     ;; just(6)
     ;; macroexpand
     (.bind (just 1)
            (fn [a]
              (.bind (just (inc a))
                     (fn [b]
                       (.bind (just (inc b))
                              (fn [c]
                                (just (+ a b c))))))))
     ]]
  (let [$ (hy.gensym)]
    (match bindings
           #() `(do ~@body)
           #(l r #* bindings) (let [body `(mlet [~@bindings] ~@body)]
                                (cond (= l :setv)
                                      (do
                                        `(let ~r ~body))
                                      (symbol? l)
                                      (do
                                        `(.bind ~r (fn [~l] ~body)))
                                      (hyseq? l)
                                      (do
                                        `(.bind ~r (fn [~$] (let [~l ~$] ~body))))
                                      True
                                      (do
                                        (raise TypeError))))
           _ (raise IndexError))))



(defclass monad []
  ;; applicative
  (defn __mul__ [self mv]
    (.apply self mv))

  ;; monad
  (defn __or__ [self fm]
    (.bind self fm))

  (defn [staticmethod] fapply [f #* mvs]
    (match mvs
           #() (f)
           #(mv #* mvs) (* (.map mv f) #* mvs)))

  (defn [classmethod] wrap [cls [v None]]
    (raise NotImplementedError))

  (defn [classmethod] zero [cls]
    (cls.wrap))

  (defn unwrap [self]
    (raise NotImplementedError))

  ;; functor
  (defn map [self f]
    (raise NotImplementedError))

  ;; applicative
  (defn apply [self mv]
    (raise NotImplementedError))

  ;; monad
  (defn bind [self fm]
    (raise NotImplementedError)))


;;; mixins

(defclass zero-monad-mixin []
  (defn __bool__ [self]
    False))

(defclass [(dataclass :slots True)] empty-monad-mixin [:metaclass singleton-meta])

(defclass [(dataclass :order True :slots True)] data-monad-mixin []
  (setv #^ Any data None))

(defclass fn-monad-mixin [data-monad-mixin]
  (defn __call__ [self #* args #** kwargs]
    (self.data #* args #** kwargs)))


;;; mid

(defclass mid [data-monad-mixin monad]
  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (mid v))

  (defn unwrap [self]
    (match self
           (mid x) x
           _ (raise TypeError)))


  (defn map [self f]
    (match self
           (mid v) (mid (f v))
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (mid f) (.map mv f)
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (mid v) (fm v)
           _ (raise TypeError))))


;;; maybe

(defclass maybe [monad]
  (defn [classmethod] wrap [cls [v None]]
    (if (none? v) (nothing) (just v)))

  (defn unwrap [self]
    (match self
           (just v) v
           (nothing) None
           _ (raise TypeError)))

  (defn map [self f]
    (match self
           (just v) (just (f v))
           (nothing) self
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (just f) (.map mv f)
           (nohting) self
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (just v) (fm v)
           (nothing) self
           _ (raise TypeError))))

(defclass just [data-monad-mixin maybe]
  (setv __slots__ #()))

(defclass nothing [zero-monad-mixin empty-monad-mixin maybe]
  (setv __slots__ #()))


;;; either

(defclass either [monad]
  (defn [classmethod] wrap [cls [v None]]
    (right v))

  (defn [classmethod] zero [cls]
    (left))

  (defn unwrap [self]
    (match self
           (right v) v
           (left v) v
           _ (raise TypeError)))

  (defn map [self]
    (match self
           (right v) (right (f v))
           (left _) self
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (right f) (.map mv f)
           (left _) self
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (right v) (fm v)
           (left _) self
           _ (raise TypeError))))

(defclass right [data-monad-mixin either]
  (setv __slots__ #()))

(defclass left [zero-monad-mixin data-monad-mixin either]
  (setv __slots__ #()))


;;; mtry

(defmacro mtry! [#* body]
  `(mtry.run (fn [] ~@body)))

(defclass mtry [monad]
  (defn [classmethod] run [cls f]
    (try
      (success (f))
      (except [e Exception]
        (failure e))))

  (defn [classmethod] wrap [cls [v None]]
    (success v))

  (defn [classmethod] zero [cls]
    (failure (Exception)))

  (defn unwrap [self]
    (match self
           (success v) v
           (failure e) (raise e)
           _ (raise TypeError)))

  (defn map [self f]
    (match self
           (success v) (mtry! (f v))
           (failure _) self
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (success f) (.map mv f)
           (failure _) self
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (success v) (fm v)
           (failure _) self
           _ (raise TypeError))))

(defclass success [data-monad-mixin mtry]
  (setv __slots__ #()))

(defclass failure [zero-monad-mixin data-monad-mixin mtry]
  (setv __slots__ #()))


;;; state

(defmacro state! [#* body]
  `(state (fn [s] ~@body)))

;; s->(v,s)
(defclass state [fn-monad-mixin monad]
  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (state! #(v s)))

  ;; mt :: s->(t,s)
  ;; self :: s->(a,s) :: ma
  ;; fm :: a->(s->(b,s)) :: a->mb
  ;; self->fm->(s->(b,s)) :: self->fm->mb
  (defn bind [self fm]
    (state!
      (let [#(v ns) (self s)]
        ((fm v) ns)))))


;;; cont

(defmacro cont! [#* body]
  `(cont (fn [k] ~@body)))

;; (v->r)->r
(defclass cont [fn-monad-mixin monad]
  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (cont! (k v)))

  ;; mt :: (t->r)->r
  ;; self :: (a->r)->r :: ma
  ;; fm :: a->((b->r)->r) :: a->mb
  ;; self->fm->((b->r)->r) :: self->fm->mb
  (defn bind [self fm]
    (cont! (self (fn [v] ((fm v) k))))))

;; mt :: (t->r)->r
;; f :: (a->((b->r)->r))->((a->r)->r) :: (a->mb)->ma
;; f->((a->r)->r) :: f->ma
(defn callCC [f]
  (cont!
    ;; v :: a
    ;; k :: a->r
    ;; exit :: a->((b->r)->r) :: a->mb
    (let [exit (fn [v] (cont (fn [_] (k v))))]
      ;; f :: (a->mb)->ma
      ;; exit :: a->mb
      ;; (f exit) :: (a->r)->r :: ma
      ;; k :: a->r
      ((f exit) k))))

(defmacro callCC! [exit #* body]
  `(callCC (fn [~exit] ~@body)))



(export
  :objects [monad mid maybe just nothing either right left mtry success failure state cont callCC]
  :macros [alet mlet mtry! state! cont! callCC!])
