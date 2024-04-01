(require
  dash.core.polyfill *
  dash.core.monad *)

(import
  dash.core.polyfill *
  dash.core.monad *)


;;; box

(defn box-t [name m]
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

(defn maybe-t [name m]
  (type name #(box)
        {"__slots__" #()
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
  (type name #(box)
        {"__slots__" #()
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
  (import dash.core.monad [fn-monad-mixin])
  (type name #(fn-monad-mixin monad)
        {"__slots__" #()
         "wrap" (classmethod
                  (fn [cls [v None]]
                    (cls (fn [s] (m.wrap #(v s))))))
         "unwrap" (fn [self]
                    (m.unwrap (self (dict))))
         "bind" (fn [self fm]
                  (self.__class__ (fn [s]
                                    (m.bind (self s)
                                            (fn [v]
                                              (let [#(v s) v]
                                                ((fm v) s)))))))}))



(export
  :objects [box-t maybe-t either-t state-t])
