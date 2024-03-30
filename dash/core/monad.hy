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
      :functor     [monad.map]
      :applicative [alet fapply monad.apply *]
      :monad       [mlet monad.bind |]}

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
     (.apply (box inc) (box 1))    ;; (box 2)
     (.apply (just inc) (just 1))  ;; (just 2)
     (.apply (just inc) (nothing)) ;; (nothing)
     :::applicative.complex
     ...
     (-> (just (curry 2 +))
         (.apply (just 1))
         (.apply (just 2)))
     ...
     (* (just (curry 2 +)) (just 1) (just 2))
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

  (defn __mul__ [self mv]
    (.apply self mv))

  (defn __or__ [self fm]
    (.bind self fm))

  (defn [classmethod] wrap [cls [v None]]
    (raise NotImplementedError))

  (defn [classmethod] zero [cls]
    (cls.wrap))

  (defn unwrap [self]
    (raise NotImplementedError))

  (defn map [self f]
    (raise NotImplementedError))

  (defn apply [self mv]
    (raise NotImplementedError))

  (defn bind [self fm]
    (raise NotImplementedError)))

(defn monad? [x] (isinstance x monad))

(defn fapply [f #* mvs]
  (match mvs
         #() (f)
         #(mv #* mvs) (* (.map mv f) #* mvs)))


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

;; also known as id monad

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
  (->> v
       (do (setv x.data v))))

(defn swap! [x f #* args #** kwargs]
  (let [v (f x.data #* args #** kwargs)]
    (reset! x v)))


;;; maybe

(defclass maybe [monad]
  #[["Maybe Monad, represent a maybe nothing (nil/null/none) result."

     ::operations
     [just nothing maybe? just? nothing?]

     ::example
     (just 1)                              ;; (just 1)
     (nothing)                             ;; (nothing)
     (| (just 1) (fn [v] (just (inc v))))  ;; (just 2)
     (| (nothing) (fn [v] (just (inc v)))) ;; (nothing)
     ]]

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

(defn maybe? [x] (isinstance x maybe))
(defn just? [x] (isinstance x just))
(defn nothing? [x] (isinstance x nothing))


;;; either

(defclass either [monad]
  #[["Either Monad, like maybe but the error branch carries data (exception/error/code)."

     ::operations
     [right left either? right? left?]

     ::example
     (right 1)                                 ;; (right 1)
     (left "404")                              ;; (left "404")
     (| (right 1) (fn [v] (right (inc v))))    ;; (right 2)
     (| (left "404") (fn [v] (right [inc v]))) ;; (left "404")
     ]]

  (defn [classmethod] wrap [cls [v None]]
    (right v))

  (defn [classmethod] zero [cls]
    (left))

  (defn unwrap [self]
    (match self
           (right v) v
           (left _) None
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

(defn either? [x] (isinstance x either))
(defn right? [x] (isinstance x right))
(defn left? [x] (isinstance x left))


;;; mtry

(defmacro mtry! [#* body]
  `(mtry.run (fn [] ~@body)))

(defclass mtry [monad]
  #[["Try Monad, also known as Error Monad or Exception Monad, like either, but:"
     ["1. In addition to explicitly returning errors, exceptions of map/bind will also ehter the error branch."
      "2. Error data is limited to exceptions, and is raised when unwrap."]

     ::operations
     [mtry! success failure mtry? success? failure? mtry.run]

     ::example
     (mtry! 1)                                        ;; (success 1)
     (mtry! (/ 1 0))                                  ;; (failure ZeroDivisionError)
     (| (success 2) (fn [v] (mtry! (/ 4 v))))         ;; (success 2)
     (| (success 0) (fn [v] (mtry! (/ 4 v))))         ;; (failure ZeroDivisionError)
     (| (failure TypeError) (fn [v] (mtry! (/ 4 v)))) ;; (failure TypeError)
     ]]

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

(defn mtry? [x] (isinstance x mtry))
(defn success? [x] (isinstance x success))
(defn failure? [x] (isinstance x failure))


;;; delay

(defmacro delay! [#* body]
  `(delay (fn [] ~@body)))

(defclass [(dataclass :slots True)] delay [monad]
  #[["Lazy computing wrapper."

     ::operations
     [delay! delay? realized? realize]

     ::example
     (setv d (delay! (+ 1 1)))  ; (delay :unrealized ...)
     (realized? d)              ; False
     (realize! d)               ; 2
     d                          ; (delay :realized 2)
     ]]

  #^ Any data
  (setv #^ bool realized False)

  (defn [classmethod] wrap [cls [v None]]
    (delay v True))

  (defn unwrap [self]
    (match self
           (delay v True) v
           (delay f False) (f)
           _ (raise TypeError)))

  (defn map [self f]
    (delay! (f (.unwrap self))))

  (defn apply [self mv]
    (.map mv (fn [x] ((.unwrap self) x))))

  (defn bind [self fm]
    (delay! (.unwrap (fm (.unwrap self))))))

(defn delay? [x] (isinstance x delay))
(defn realized? [x] x.realized)

(defn realize! [x]
  (->> x.data
       (do
         (unless (realized? x)
           (setv #(x.data x.realized) #((x.data) True))))))


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
  :objects [
            ;; monad
            monad monad? fapply
            ;; box
            box box? reset! swap!
            ;; maybe
            maybe just nothing maybe? just? nothing?
            ;; either
            either right left either? right? left?
            ;; mtry
            mtry success failure mtry? success? failure?
            ;; delay
            delay delay? realized? realize!
            ;; state
            state state?
            ;; cont
            cont cont? callCC
            ]
  :macros [alet mlet mtry! delay! state! cont! callCC!])
