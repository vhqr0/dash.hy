(setv __doc__
      #[["State Monad, represents a state transition function."
         ["That accepts an initial state and returns the result and final state."]

         ::types
         :state-t [s->m(a,s)]
         :state [s->(a,s)] ;; (state-t :m monad-id)

         ::operations
         [state! state state? state-t]

         ::examples
         (defn f [s] #(1 (inc s)))
         (state f)                                        ;; (state f)
         (state! (f s))                                   ;; (state (fn [s] (f s)))
         (state.wrap 1)                                   ;; (state! #(1 s))
         (.unwrap (state f) 1)                            ;; #(1 2)
         (.unwrap (.map (state f) inc) 1)                 ;; #(2 2)
         (.unwrap (.bind (state f) (fn [v] (state f))) 1) ;; #(1 3)
         ]])

(require
  dash.core *)

(import
  dash.core *
  dash.core.monad [data-monad-mixin monad-id])

(defn state-t [name m]
  (setx wrapper
        (type name #(data-monad-mixin monad)
              {"__slots__" #()
               "wrap" (classmethod
                        (fn [cls v]
                          (wrapper (fn [s] (m.wrap #(v s))))))
               "unwrap" (fn [self [s None]]
                          (match self
                                 (wrapper f) (m.unwrap (f s))
                                 _ (raise TypeError)))
               "map" (fn [self f]
                       (match self
                              (wrapper g) (wrapper
                                            (fn [s]
                                              (m.map (g s)
                                                     (fn [v]
                                                       (let [#(v s) v]
                                                         #((f v) s))))))
                              _ (raise TypeError)))
               ;; self :: [s->m(a,s)]
               ;; fm :: a->[s->m(b,s)]
               ;; self->fm->[s->m(b,s)]
               "bind" (fn [self fm]
                        (match self ; self :: [s->m(a,s)]
                               (wrapper f) ; f :: s->m(a,s)
                               (do
                                 (wrapper
                                   (fn [s]
                                     (m.bind (f s) ; (f s) :: m(a,s)
                                             (fn [v] ; v :: (a,s)
                                               (let [#(v s) v] ; #(v s) :: #(a s)
                                                 (match (fm v) ; (fm v) :: [s->m(b,s)]
                                                        (wrapper f) (f s) ; (f s) :: m(b,s)
                                                        _ (raise TypeError))))))))
                               _ (raise TypeError)))})))

(defmacro state! [#* body]
  `(state (fn [s] ~@body)))

(setv state (state-t "state" monad-id)
      state.__doc__ __doc__)

(defn state? [x] (isinstance x state))

(export
  :objects [state state? state-t]
  :macros [state!])
