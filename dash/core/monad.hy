(setv __doc__
      #[["Monad."

         :toc
         [::wrapper
          :::wrapper.operations
          :::wrapper.examples

          ::functor
          :::functor.operations
          :::functor.examples

          ::applicative
          :::applicative.operations
          :::applicative.examples
          :::applicative.alet

          ::monad
          :::monad.operations
          :::monad.examples
          :::monad.mlet

          ::box
          :::box.operations
          :::box.examples
          :::box.transformer

          ::either
          :::either.operations
          :::either.examples

          ::delay
          :::delay.operations
          :::delay.examples
          ]

         ================================================================================

         ::wrapper
         "Data wrapper."

         :::wrapper.operations
         [monad? monad.wrap monad.zero monad.unwrap]

         :::wrapper.examples
         (either.wrap 1)      ;; (right 1)
         (either.zero)        ;; (left)
         (.unwrap (right 1))  ;; 1
         (.unwrap (left))     ;; (raise Exception)

         ================================================================================

         ::functor
         "Isomorphic data structure, which can be mapped by function."

         :::functor.operations
         [fmap monad.map]

         :::functor.examples
         (.map (box 1) inc)   ;; (box 2)
         (.map (right 1) inc) ;; (right 2)
         (.map (left) inc)    ;; (left)
         (fmap inc (box 1))   ;; (box 2)
         (fmap inc (right 1)) ;; (right 2)
         (fmap inc (left))    ;; (left)

         ================================================================================

         ::applicative
         "Functor that wraps a function."

         :::applicative.operations
         [alet fapply monad.apply]

         :::applicative.examples
         (setv add2 (curry 2 +))
         (.apply (box inc) (box 1))          ;; (box 2)
         (.apply (right inc) (right 1))      ;; (right 2)
         (.apply (right inc) (left))         ;; (left)
         (.apply (left) (right 1))           ;; (left)
         (fmap inc)                          ;; (curry 2 fmap inc)
         (fmap (fmap inc))                   ;; (curry 2 fmap (curry 2 fmap inc))
         ((fmap inc) (box 1))                ;; (box 2)
         ((fmap inc) (right 1))              ;; (right 2)
         ((fmap inc) (left))                 ;; (left)
         ((fmap (fmap inc)) (right (box 1))) ;; (right (box 2))
         ((fmap (fmap inc)) (left))          ;; (left)
         (fapply add2 (box 1) (box 2))       ;; (box 3)
         (fapply add2 (right 1) (right 2))   ;; (right 3)
         (fapply add2 (right 1) (left))      ;; (left)

         :::applicative.alet
         ["Applicative functor let."
          "1. Parallel computing and binding functors `fvs'."
          "2. Wrap body to a curried applicative functor `af'."
          "3. Apply functors `fvs' to applicative functor `af'."]

         ::::applicative.alet.examples

         :input
         (alet [a (right 1)
                b (right 2)
                c (right 3)]
           (+ a b c))

         :output
         (let [a (right 1)
               b (right 2)
               c (right 3)]
           (-> (fn [a] (fn [b] (fn [c] (+ a b c))))
               (fapply a b c)))

         ================================================================================

         ::monad
         "Control flow composition."

         :::monad.operations
         [mlet monad.bind]

         :::monad.examples
         (.bind (box 1) (comp box inc))     ;; (box 2)
         (.bind (right 1) (comp right inc)) ;; (box 2)
         (.bind (right) (comp right inc))   ;; (left)

         :::monad.mlet
         ["Monad let."
          "Nested to create a new monad, and then bind to `fm' as name."]

         ::::monad.mlet.examples

         :input
         (mlet [a (right 1)
                b (right (inc a))
                c (right (inc b))]
           (right (+ a b c)))

         :output
         (.bind (right 1)
                (fn [a]
                  (.bind (right (inc a))
                         (fn [b]
                           (.bind (right (inc b))
                                  (fn [c]
                                    (right (+ a b c))))))))

         ================================================================================

         ::box
         "Box Monad, right boxing a value, also known as ID Monad."

         :::box.operations
         [box-t box box? reset! swap!]

         :::box.examples
         (box 1)                        ;; (box 1)
         (box.wrap 1)                   ;; (box 1)
         (.map (box 1) inc)             ;; (box 2)
         (.apply (box inc) (box 1))     ;; (box 2)
         (.bind (box 1) (comp box inc)) ;; (box 2)
         (setv b (box 1))               ;; (box 1)
         (swap! b inc)                  ;; (box 2)
         (swap! b + 2)                  ;; (box 4)
         (reset! b 0)                   ;; (box 0)

         :::box.transformer
         "Boxing an existing monad."

         ::::box.transformer.examples

         :delay
         (setv delaybox (box-t "delaybox" delay))
         (delaybox.wrap 1)               ;; (delaybox (now 1))
         (setv db (delaybox (later! 1))) ;; (delaybox (later (fn [] 1)))
         (swap! db identity)             ;; (later (fn [] 1))
         (swap! db force)                ;; (now 1)
         (swap! db identity)             ;; (now 1)

         :either
         (setv eitherbox (box-t "eitherbox" either))
         (eitherbox.wrap 1)                    ;; (eitherbox (right 1))
         (eitherbox.zero)                      ;; (eitherbox (left))
         (setv eb (eitherbox.zero))            ;; (eitherbox (left))
         (swap! eb identity)                   ;; (left)
         (swap! eb except- Exception identity) ;; (right (Exception))
         (swap! eb identity)                   ;; (right (Exception))

         ================================================================================

         ::either
         "Either Monad, represents a result or failure."
         ["Also known as Try Monad, Error Monad or Except Monad."
          "When failure is None, also known as Maybe Monad or Optional Monad."]

         :::either.operations
         [try! except! right left either? right? left? try- except-]

         :::either.examples
         (defn div4 [n] (try! (/ 4 n)))
         (right 1)                             ;; (right 1)
         (left)                                ;; (left)
         (left 404)                            ;; (left 404)
         (either.wrap 1)                       ;; (right 1)
         (either.zero)                         ;; (left)
         (try! (/ 4 2))                        ;; (right 2)
         (try! (/ 4 0))                        ;; (left ZeroDivisionError)
         (div4 2)                              ;; (right 2)
         (div4 0)                              ;; (left ZeroDivisionError)
         (except! (div4 2) Exception identity) ;; (right 2)
         (except! (div4 0) Exception identity) ;; (right (ZeroDivisionError))
         (.map (right 1) inc)                  ;; (right 2)
         (.map (left) inc)                     ;; (left)
         (.map (left 404) inc)                 ;; (left 404)
         (.apply (right inc) (right 1))        ;; (right 2)
         (.apply (right inc) (left))           ;; (left)
         (.apply (right inc) (left 404))       ;; (left 404)
         (.apply (left) (right 1))             ;; (left)
         (.apply (left 404) (right 1))         ;; (left 404)
         (.apply (left 404) (left 401))        ;; (left 404)
         (.bind (right 1) (comp right inc))    ;; (right 2)
         (.bind (left) (comp right inc))       ;; (left)
         (.bind (left 404) (comp right inc))   ;; (left 404)
         (.bind (right 2) div4)                ;; (right 2)
         (.bind (right 0) div4)                ;; (left ZeroDivisionError)
         (.bind (left 404) div4)               ;; (left 404)

         ================================================================================

         ::delay
         "Delay Monad, represents a result of delay calculation."
         ["The final result can be unwrap, or force a delay to now."]

         :::delay.operations
         [later! now later delay? now? later? force]

         :::delay.examples
         (now 1)                                 ;; 1
         (later (fn [] (+ 1 2)))                 ;; (later (fn [] (+ 1 2)))
         (later! (+ 1 2))                        ;; (later (fn [] (+ 1 2)))
         (delay.wrap 1)                          ;; (now 1)
         (force (later! (+ 1 2)))                ;; (now 3)
         (.map (now 1) inc)                      ;; (now 2)
         (.map (later! (+ 1 2)) inc)             ;; (later (fn [] (inc (+ 1 2))))
         (.apply (now inc) (now 1))              ;; (now 2)
         (.apply (later! inc) (now 1))           ;; (later (fn [] (((fn [] inc)) 1)))
         (.bind (now 1) (comp now inc))          ;; (now 2)
         (.bind (later! (+ 1 2)) (comp now inc)) ;; (later (fn [] (delay.unwrap (now (inc ((fn [] (+ 1 2))))))))

         ================================================================================
         ]])


;;; monad

(require
  dash.core.polyfill *)

(import
  dash.core.polyfill *
  dataclasses [dataclass]
  typing [Any])

(defclass monad []
  (setv __doc__ __doc__)

  (defn [classmethod] wrap [cls v]
    (raise NotImplementedError))

  (defn [classmethod] zero [cls]
    (raise NotImplementedError))

  (defn unwrap [self]
    (raise NotImplementedError))

  (defn map [self f]
    (raise NotImplementedError))

  (defn apply [self mv]
    (raise NotImplementedError))

  (defn bind [self fm]
    (raise NotImplementedError)))

(defn monad? [x] (isinstance x monad))

(defn [(curry 2)] fmap [f mv]
  (.map mv f))

(defn fapply [f #* mvs]
  (ap-reduce (.apply acc it) (box f) mvs))

(defmacro alet [bindings #* body]
  (import itertools [batched])
  (let [ls (->> (batched bindings 2) (ap-map (let [#(l r) it] l)) list)]
    (assert (all (map symbol? ls)))
    `(let ~bindings
       (-> ~(ap-reduce `(fn [~it] ~acc) `(do ~@body) (reversed ls))
           (fapply ~@ls)))))

(defmacro mlet [bindings #* body]
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

(defclass [(dataclass :order True :slots True)] data-monad-mixin []
  (setv #^ Any data None))

(defclass monad-id [monad]
  (defn [classmethod] wrap [cls v]
    v)

  (defn unwrap [self]
    self)

  (defn map [self f]
    (f self))

  (defn apply [self mv]
    (self mv))

  (defn bind [self fm]
    (fm self)))


;;; box

(defn box-t [name m]
  (type name #(data-monad-mixin)
        {"__slots__" #()
         "wrap" (classmethod
                  (fn [cls v]
                    (cls (m.wrap v))))
         "zero" (classmethod
                  (fn [cls]
                    (cls (m.zero))))
         "unwrap" (fn [self]
                    (m.unwrap self.data))
         "map" (fn [self f]
                 (self.__class__ (m.map self.data f)))
         "apply" (fn [self mv]
                   (mv.__class__ (m.apply self.data mv.data)))
         "bind" (fn [self fm]
                  (self.__class__ (m.bind self.data (fn [v] (. (fm v) data)))))}))

(setv box (box-t "box" monad-id))

(defn box? [x] (isinstance x box))

(defn reset! [x v]
  (->> v (do (setv x.data v))))

(defn swap! [x f #* args #** kwargs]
  (let [v (f x.data #* args #** kwargs)]
    (reset! x v)))

(defmacro ap-swap! [x form]
  `(swap! ~x (fn [it] ~form)))


;;; either

(defclass either [monad]
  (defn [classmethod] wrap [cls v]
    (right v))

  (defn [classmethod] zero [cls]
    (left))

  (defn unwrap [self]
    (match self
           (right v) v
           (left e) (raise (raisible e))
           _ (raise TypeError)))

  (defn map [self f]
    (match self
           (right v) (right (f v))
           (left _) self
           _ (raise TypeError)))

  (defn apply [self mv]
    (match self
           (right f) (either.map mv f)
           (left _) self
           _ (raise TypeError)))

  (defn bind [self fm]
    (match self
           (right v) (fm v)
           (left _) self
           _ (raise TypeError))))

(defclass right [data-monad-mixin either]
  (setv __slots__ #()))

(defclass left [data-monad-mixin either]
  (setv __slots__ #()))

(defn either? [x] (isinstance x either))
(defn right? [x] (isinstance x right))
(defn left? [x] (isinstance x left))

(defn try- [f]
  (try
    (right (f))
    (except [e Exception]
      (left e))))

(defn except- [x excs f]
  (try-
    (fn []
      (try
        (either.unwrap x)
        (except [e excs]
          (f e))))))

(defmacro try! [#* body]
  `(try- (fn [] ~@body)))

(defmacro except! [x excs #* body]
  `(except- ~x ~excs (fn [e] ~@body)))


;;; delay

(defmacro later! [#* body]
  `(later (fn [] ~@body)))

(defclass delay [monad]
  (defn [classmethod] wrap [cls v]
    (now v))

  (defn unwrap [self]
    (match self
           (now v) v
           (later f) (f)
           _ (raise TypeError)))

  (defn map [self f]
    (match self
           (now v) (now (f v))
           (later g) (later! (f (g)))
           _ (raise TypeError)))

  (defn apply [self mv]
    (delay.map mv
               (match self
                      (now f) f
                      (later g) (fn [x] ((g) x))
                      _ (raise TypeError))))

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



(export
  :objects [
            ;; monad
            monad monad? fmap fapply
            ;; box
            box-t box box? reset! swap!
            ;; either
            either right left either? right? left? try- except-
            ;; delay
            delay now later delay? now? later? force
            ]
  :macros [alet mlet ap-swap! try! except! later!])
