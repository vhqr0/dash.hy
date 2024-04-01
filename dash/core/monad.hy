(require
  dash.core.polyfill *)

(import
  dash.core.polyfill *
  dash.metaclasses [singleton-meta]
  dataclasses [dataclass]
  typing [Any])


;;; monad

(defclass monad []
  #[["Monad."

     ::operations
     {:wrap        [monad? monad.wrap moand.zero monad.unwrap]
      :functor     [fmap monad.map]
      :applicative [alet fapply monad.apply]
      :monad       [mlet monad.bind]}

     ::wrap
     (box.wrap 1)        ;; (box 1)
     (maybe.wrap 1)      ;; (just 1)
     (maybe.zero)        ;; (nothing)
     (.unwrap (just 1))  ;; 1
     (.unwrap (nothing)) ;; None

     ::functor
     (.map (box 1) inc)   ;; (box 2)
     (.map (just 1) inc)  ;; (just 2)
     (.map (nohting) inc) ;; (nothing)

     ::applicative
     (.apply (box inc) (box 1))          ;; (box 2)
     (.apply (just inc) (just 1))        ;; (just 2)
     (.apply (just inc) (nothing))       ;; (nothing)
     ((box inc) (box 1))                 ;; (box 2)
     ((just inc) (just 1))               ;; (just 2)
     ((just inc) (nothing))              ;; (nothing)
     ((fmap inc) (box 1))                ;; (box 2)
     ((fmap inc) (just 1))               ;; (just 2)
     ((fmap inc) (nothing))              ;; (nothing)
     ((box (just inc)) (box (just 1)))   ;; (box (just 2))
     ((box (just inc)) (box (nothing)))  ;; (box (nothing))
     ((box (fmap inc)) (box (just 1)))   ;; (box (just 2))
     ((fmap (just inc)) (box (just 1)))  ;; (box (just 2))
     ((fmap (fmap inc)) (box (just 1)))  ;; (box (just 2))
     ((fmap (fmap inc)) (box (nothing))) ;; (box (nothing))
     ((fmap (fmap inc)) (nothing))       ;; (nothing)
     :::applicative.complex
     ...
     (-> (just (curry 2 +))
         (.apply (just 1))
         (.apply (just 2)))
     ...
     (fapply (curry 2 +) (just 1) (just 2))
     ...
     (alet [a (just 1)
            b (just 2)]
       (+ a b))
     ...

     ::monad
     (.bind (box 1) (fn [v] (box (inc v))))   ;; (box 2)
     (.bind (just 1) (fn [v] (just (inc v)))) ;; (box 2)
     (.bind (noting) (fn [v] (just (inc v)))) ;; (nothing)
     :::monad.complex
     ...
     (.bind (just 1)
            (fn [a]
              (.bind (just (inc a))
                     (fn [b]
                       (just (+ a b))))))
     ...
     (mlet [a (just 1)
            b (just (inc a))]
       (just (+ a b)))
     ...
     ]]

  ;; Applicative functor itself is also a lifted function (ma->mb).
  ;; Therefore the constructor is a corresponding lifting function ((a->b)->(ma->mb)).
  (defn __call__ [self mv]
    (.apply self mv))

  (defn [classmethod] wrap [cls [v None]]
    (raise NotImplementedError))

  (defn [classmethod] zero [cls]
    (raise NotImplementedError))

  (defn unwrap [self]
    (raise NotImplementedError))

  (defn [classmethod] lift [cls f]
    (raise NotImplementedError))

  (defn map [self f]
    (raise NotImplementedError))

  (defn apply [self mv]
    (raise NotImplementedError))

  (defn bind [self fm]
    (raise NotImplementedError)))

(defn monad? [x] (isinstance x monad))

;; fmap is a lifting function whose type cannot be determined until
;; the mv is received.
(defn [(curry 2)] fmap [f mv]
  (.map mv f))

(defn fapply [f #* mvs]
  (ap-reduce (acc it) (fmap f) mvs))


;;; let macros

(defmacro alet [bindings #* body]
  #[["Applicative functor let."
     ["1. Parallel computing and binding functors `fvs'."
      "2. Wrap body to a curried applicative functor `af'."
      "3. Apply functors `fvs' to applicative functor `af'."]

     ::example

     :::example.input
     (alet [a (just 1)
            b (just 2)
            c (just 3)]
       (+ a b c))

     :::example.output
     (let [a (just 1)
           b (just 2)
           c (just 3)]
       (-> (fn [a] (fn [b] (fn [c] (+ a b c))))
           (fapply a b c)))
     ]]

  (import itertools [batched])

  (let [ls (->> (batched bindings 2) (ap-map (let [#(l r) it] l)) list)]
    (assert (all (map symbol? ls)))
    `(let ~bindings
       (-> ~(ap-reduce `(fn [~it] ~acc) `(do ~@body) (reversed ls))
           (fapply ~@ls)))))

(defmacro mlet [bindings #* body]
  #[["Monad let."
     ["Nested to create a new monad, and then bind to `fm' as name."]

     ::example

     :::example.input
     (mlet [a (just 1)
            b (just (inc a))
            c (just (inc b))]
       (just (+ a b c)))

     :::example.output
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


;;; box

(defclass box [data-monad-mixin monad]
  #[["Box Monad, just boxing a vlaue, also known as ID Monad."

     ::operations
     [box? reset! swap!]

     ::example
     (setv b (box 1)) ;; (box 1)
     (swap! b inc)    ;; (box 2)
     (swap! b + 2)    ;; (box 4)
     (reset! b 0)     ;; (box 0)
     ]]

  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (box v))

  (defn unwrap [self]
    (match self
           (box x) x
           _ (raise TypeError)))

  (defn [classmethod] lift [cls f]
    (fn [x] (box.map x f)))

  (defn map [self f]
    (match self
           (box v) (box (f v))
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (box f) (.map mv f)
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (box v) (fm v)
           _ (raise TypeError))))

(defn box? [x] (isinstance x box))

(defn reset! [x v]
  (->> v (do (setv x.data v))))

(defn swap! [x f #* args #** kwargs]
  (let [v (f x.data #* args #** kwargs)]
    (reset! x v)))

(defmacro ap-swap! [x form]
  `(swap! ~x (fn [it] ~form)))

(defn boxed-monad [name m]
  (type name #(box)
        {"__slots__" #()
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (m.wrap v))))
         "zero" (classmethod
                  (fn [cls]
                    (cls (m.zero))))
         "unwrap" (fn [self]
                    (m.unwrap self.data))
         "lift" (classmethod
                  (fn [cls f]
                    (fn [x] (self.__class__ (f self.data)))))
         "map" (fn [self f]
                 (self.__class__ (m.map self.data f)))
         "apply" (fn [self mv]
                   (m.apply self.data mv))
         "bind" (fn [self fm]
                  (let [cls self.__class__]
                    (cls
                      (m.bind self.data
                              (fn [v]
                                (match (fm v)
                                       (cls v) v
                                       _ (raise (TypeError))))))))}))


;;; maybe

(defclass maybe [monad]
  #[["Maybe Monad, represent a result maybe nothing (nil/null/none)."

     ::operations
     [just nothing maybe? just? nothing?]

     ::example
     (just 1)                                  ;; (just 1)
     (nothing)                                 ;; (nothing)
     (.bind (just 1) (fn [v] (just (inc v))))  ;; (just 2)
     (.bind (nothing) (fn [v] (just (inc v)))) ;; (nothing)
     ]]

  (defn [classmethod] wrap [cls [v None]]
    (just v))

  (defn [classmethod] zero [cls]
    (nothing))

  (defn unwrap [self]
    (match self
           (just v) v
           (nothing) None
           _ (raise TypeError)))

  (defn [classmethod] lift [cls f]
    (fn [x] (maybe.map x f)))

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

(defn maybe? [x] (isinstance x maybe))
(defn just? [x] (isinstance x just))
(defn nothing? [x] (isinstance x nothing))


;;; either

(defclass either [monad]
  #[["Either Monad, like maybe but the error branch carries data (errcode/error/exception)."

     ::operations
     [try! except! success failure either? success? failure? try- except-]

     ::example
     (success 1)                                         ;; (success 1)
     (failure "404")                                     ;; (failure "404")
     (try! 1)                                            ;; (success 1)
     (try! (/ 1 0))                                      ;; (failure ZeroDivisionError)
     (.bind (success 1) (fn [v] (success (inc v))))      ;; (success 2)
     (.bind (failure "404") (fn [v] (success [inc v])))  ;; (failure "404")
     (.bind (success 2) (fn [v] (try! (/ 4 v))))         ;; (success 2)
     (.bind (success 0) (fn [v] (try! (/ 4 v))))         ;; (failure ZeroDivisionError)
     (.bind (failure TypeError) (fn [v] (try! (/ 4 v)))) ;; (failure TypeError)
     ]]

  (defn [classmethod] wrap [cls [v None]]
    (success v))

  (defn [classmethod] zero [cls]
    (failure))

  (defn unwrap [self]
    (match self
           (success v) v
           (failure e) (raise (cond (raisible? e) e
                                    (none? e) (Exception)
                                    True (Exception e)))
           _ (raise TypeError)))

  (defn [classmethod] lift [cls f]
    (fn [x] (either.map x f)))

  (defn map [self f]
    (match self
           (success v) (success (f v))
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

(defclass success [data-monad-mixin either]
  (setv __slots__ #()))

(defclass failure [zero-monad-mixin data-monad-mixin either]
  (setv __slots__ #()))

(defn either? [x] (isinstance x either))
(defn success? [x] (isinstance x success))
(defn failure? [x] (isinstance x failure))

(defmacro try! [#* body]
  `(try- (fn [] ~@body)))

(defmacro except! [x excs #* body]
  `(except- ~x ~excs (fn [e] ~@body)))

(defn try- [f]
  (try
    (success (f))
    (except [e Exception]
      (failure e))))

(defn except- [x excs f]
  (try!
    (try
      (either.unwrap x)
      (except [e excs]
        (f e)))))


;;; delay

(defmacro later! [#* body]
  `(later (fn [] ~@body)))

(defclass delay [monad]
  #[["delay Monad, represent the result of a delay calculation."
     ["The final result can be unwrap, or force a delay to now."]

     ::operations
     [later! now later delay? now? later? force]

     ::example
     (later! (+ 1 2))                                ;; (later (fn [] (+ 1 2)))
     (force (later! (+ 1 2)))                        ;; (now 3)
     (.map (now 1) inc)                              ;; (now 2)
     (.map (later! (+ 1 2)) inc)                     ;; (later (fn [] (inc (+ 1 2))))
     (.bind (now 1) (fn [v] (now (inc v))))          ;; (now 2)
     (.bind (later! (+ 1 2)) (fn [v] (now (inc v)))) ;; (later (fn [] (delay.unwrap (now (inc (+ 1 2))))))
     ]]

  (defn [classmethod] wrap [cls [v None]]
    (now v))

  (defn unwrap [self]
    (match self
           (now v) v
           (later f) (f)
           _ (raise TypeError)))

  (defn [classmethod] lift [cls f]
    (fn [x] (delay.map x f)))

  (defn map [self f]
    (match self
           (now v) (now (f v))
           (later g) (later! (f (g)))
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (now f) (.map mv f)
           (later g) (.map mv (fn [x] ((g) x)))
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (now v) (fm v)
           (later f) (later! (delay.unwrap (fm (f))))
           _ (raise TypeError))))

(defclass now [data-monad-mixin delay]
  (setv __slots__ #()))

(defclass later [data-monad-mixin delay]
  (setv __slots__ #()))

(defn delay? [x] (isinstance x delay))
(defn now? [x] (isinstance x now))
(defn later? [x] (isinstance x later))
(defn force [x] (now (delay.unwrap x)))


;;; state

(defmacro state! [#* body]
  `(state (fn [s] ~@body)))

;; s->(v,s)
(defclass state [fn-monad-mixin monad]
  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (state! #(v s)))

  (defn unwrap [self]
    (self (dict)))

  ;; mt :: s->(t,s)
  ;; self :: s->(a,s) :: ma
  ;; fm :: a->(s->(b,s)) :: a->mb
  ;; self->fm->(s->(b,s)) :: self->fm->mb
  (defn bind [self fm]
    (state!
      (let [#(v ns) (self s)]
        ((fm v) ns)))))

(defn state? [x] (isinstance x state))


;;; cont

(defmacro cont! [#* body]
  `(cont (fn [k] ~@body)))

;; (v->r)->r
(defclass cont [fn-monad-mixin monad]
  (setv __slots__ #())

  (defn [classmethod] wrap [cls [v None]]
    (cont! (k v)))

  (defn unwrap [self]
    (self identity))

  ;; mt :: (t->r)->r
  ;; self :: (a->r)->r :: ma
  ;; fm :: a->((b->r)->r) :: a->mb
  ;; self->fm->((b->r)->r) :: self->fm->mb
  (defn bind [self fm]
    (cont! (self (fn [v] ((fm v) k))))))

(defn cont? [x] (isinstance x cont))

;; mt :: (t->r)->r
;; f :: (a->((b->r)->r))->((a->r)->r) :: (a->mb)->ma
;; f->((a->r)->r) :: f->ma
(defn call-cc [f]
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

(defmacro with-cc [exit #* body]
  `(call-cc (fn [~exit] ~@body)))



(export
  :objects [
            ;; monad
            monad monad? fmap fapply
            ;; box
            box box? reset! swap! boxed-monad
            ;; maybe
            maybe just nothing maybe? just? nothing?
            ;; either
            either success failure either? success? failure? try- except-
            ;; delay
            delay now later delay? now? later? force
            ;; state
            state state?
            ;; cont
            cont cont? call-cc
            ]
  :macros [alet mlet ap-swap! try! except! later! state! cont! with-cc])
