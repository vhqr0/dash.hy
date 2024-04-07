(setv __doc__
      #[["Cont Monad, represents a CPS style function."
         ["That accepts a callback and returns the result."]

         ::types
         :cont-t [(a->b)->mb]
         :cont [(a->b)->b] ;; (cont-t :m monad-id)

         ::operations
         [cont! cont cont? call-cc with-cc]

         ::examples
         (defn f [k] (k 1))
         (cont f)                                                ;; (cont f)
         (cont! (f k))                                           ;; (cont (fn [k] (f k)))
         (cont.wrap 1)                                           ;; (cont (fn [k] (k 1)))
         (.unwrap (cont f))                                      ;; 1
         (.unwrap (.map (cont f) inc))                           ;; 2
         (.unwrap (.bind (cont f) (fn [v] (cont! (k (inc v)))))) ;; 2
         ]])

(require
  dash.core *)

(import
  dash.core *
  dash.core.monad [data-monad-mixin monad-id])

(defn cont-t [name m]
  (setx wrapper
        (type name #(data-monad-mixin monad)
              {"__slots__" #()
               "wrap" (classmethod
                        (fn [cls v]
                          (wrapper (fn [k] (k v)))))
               "unwrap" (fn [self [k m.wrap]]
                          (match self
                                 (wrapper f) (m.unwrap (f k))
                                 _ (raise TypeError)))
               "map" (fn [self f]
                       (match self
                              (wrapper g) (wrapper (fn [k] (g (comp k f))))
                              _ (raise TypeError)))
               ;; self :: [(a->mr)->mr]
               ;; fm :: a->[(b->mr)->mr]
               ;; self->fm->[(b->mr)->mr]
               "bind" (fn [self fm]
                        (match self ; self :: [(a->mr)->mr]
                               (wrapper f) ; f :: (a->mr)->mr
                               (do
                                 (wrapper
                                   (fn [k] ; k :: (b->mr)
                                     (f (fn [v] ; v :: a
                                          (match (fm v) ; (fm v) :: [(b->mr)->mr]
                                                 (wrapper f) ; f :: (b->mr)->mr
                                                 (do
                                                   (f k))
                                                 _ (raise TypeError)))))))
                               _ (raise TypeError)))})))

(defmacro cont! [#* body]
  `(cont (fn [k] ~@body)))

(setv cont (cont-t "cont" monad-id)
      cont.__doc__ __doc__)

(defn cont? [x] (isinstance x cont))

;; E :: k->Ek
;; Ek :: v->Ekv
;; Ekv :: [(*->r)->r]
;; (bind Ekv *) => Ekv
;;
;; (defn [(curry 2)] E [k v]
;;   (cont (fn [_] (k v))))

;; f :: (a->[(*->r)->r])->[(a->r)->r]
;; f->[(a->r)->r]
(defn call-cc [f]
  (cont
    (fn [k] ; k :: a->r
      (let [exit ; exit :: a->[(*->r)->r]
            (fn [v] ; v :: a
              (cont (fn [_] (k v))))]
        (match (f exit) ; (f exit) :: [(a->r)->r]
               (cont g) ; (cont g) :: (a->r)->r
               (do
                 (g k)) ; (g k) :: r
               _ (raise TypeError))))))

(defmacro with-cc [exit #* body]
  `(call-cc (fn [~exit] ~@body)))

(export
  :objects [cont cont? call-cc]
  :macros [cont! with-cc])
