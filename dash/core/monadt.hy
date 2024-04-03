(require
  dash.core.polyfill *
  dash.core.monad *)

(import
  dash.core.polyfill *
  dash.core.monad *
  dash.strtools :as s)


;;; box

(defn box-t [name m]
  #[["Boxing an existing monad."

     ::example.delay
     (setv lazy (box-t "lazy" delay))
     (lazy.wrap 1)                                 ;; (lazy (now 1))
     (lazy (later! 1))                             ;; (lazy (later (fn [] 1)))
     (let [l (lazy (later! 1))] (swap! l force) l) ;; (lazy (now 1))

     ::example.either
     (setv trybox (box-t "trybox" either))
     (let [e (trybox.zero)] (swap! except- Exception identity) e) ;; (trybox (success (Exception)))
     ]]

  (type name #(box)
        {"__doc__" (s.format "(box-t {} {})" (repr name) (repr m))
         "__slots__" #()
         "__bool__" (fn [self]
                      (bool self.data))
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (m.wrap v))))
         "zero" (classmethod
                  (fn [cls]
                    (cls (m.zero))))
         "unwrap" (fn [self]
                    (m.unwrap self.data))
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

(defn maybe-t [name m]
  "Make the results of monad optional."
  (type name #(box)
        {"__doc__" (s.format "(maybe-t {} {})" (repr name) (repr m))
         "__slots__" #()
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (m.wrap (just v)))))
         "bind" (fn [self fm]
                  (let [cls self.__class__]
                    (cls (m.bind self.data
                                 (fn [v]
                                   (match v
                                          (just v) (match (fm v)
                                                          (cls v) v
                                                          _ (raise TypeError))
                                          (nothing) (m.wrap v)
                                          _ (raise TypeError)))))))}))


;;; either

(defn either-t [name m]
  "Make the results of monad may be abnormal."
  (type name #(box)
        {"__doc__" (s.format "(either-t {} {})" (repr name) (repr m))
         "__slots__" #()
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (m.wrap (success v)))))
         "bind" (fn [self fm]
                  (let [cls self.__class__]
                    (cls (m.bind self.data
                                 (fn [v]
                                   (match v
                                          (success v) (match (fm v)
                                                             (cls v) v
                                                             _ (raise TypeError))
                                          (failure _) (m.wrap v)
                                          _ (raise TypeError)))))))}))


;;; state

(defn state-t [name m]
  "Make the the results of the state monad have more traits (maybe/either/delay)."
  (import dash.core.monad [data-monad-mixin])
  (type name #(data-monad-mixin monad)
        {"__doc__" (s.format "(box-t {} {})" (repr name) (repr m))
         "__slots__" #()
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (fn [s] (m.wrap #(v s))))))
         "bind" (fn [self fm]
                  (let [cls self.__class__]
                    (cls (fn [s]
                           (m.bind (self.data s)
                                   (fn [v]
                                     (let [#(v s) v]
                                       (match (fm v)
                                              (cls f) (f s)
                                              _ (raise TypeError)))))))))}))



(export
  :objects [box-t maybe-t either-t state-t])
