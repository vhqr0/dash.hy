(require
  dash.core.polyfill *
  dash.core.monad *
  dash.core.sequence *
  dash.core.seqtools *)

(import
  dash.core.polyfill *
  dash.core.monad *
  dash.core.sequence *
  dash.core.seqtools *
  functools [singledispatch])


;;; contains?

(defn [singledispatch] contains? [c k]
  (raise TypeError))

(contains?.register
  (fn [#^ Sequence c k]
    (in k (range (len c)))))

(contains?.register
  (fn [#^ (| Mapping Set) c k]
    (in k c)))


;;; -get

(defn [singledispatch] -get [c k [default None]]
  (raise TypeError))

(-get.register
  (fn [#^ Sequence c k [default None]]
    (try
      (get c k)
      (except [IndexError]
        default))))

(-get.register
  (fn [#^ Mapping c k [default None]]
    (try
      (get c k)
      (except [KeyError]
        default))))

(-get.register
  (fn [#^ Set c k [default None]]
    (if (contains? c k) k default)))

(-get.register
  (fn [#^ seq c k [default None]]
    (try
      (nth c k)
      (except [IndexError]
        default))))

(defn -get-in [c ks [default None]]
  (match ks
         #() c
         #(k #* ks) (let [nc (-get c k)]
                      (if (none? nc)
                          default
                          (-get-in nc ks default)))))


;;; -misc

(defn -cut [c #* args]
  (import itertools [islice])
  (match args
         #() (if (sequence? c) (cut c) (iter c))
         #(stop) (if (sequence? c)
                     (cut c stop)
                     (islice c stop))
         #(start stop) (if (sequence? c)
                           (cut c start stop)
                           (islice c start stop))
         #(start stop step) (if (sequence? c)
                                (cut c start stop step)
                                (islice c start stop step))
         _ (raise IndexError)))

(defn -len [c]
  (if (countable? c)
      (len c)
      (let [acc 0]
        (->> acc
             (do (ap-doiter c (+= acc 1)))))))

(defn -reversed [c]
  (if (reversible? c)
      (reversed c)
      (reversed (list c))))


;;; assoc!

(defn assoc! [c k v]
  (->> c
       (do (setv (get c k) v))))

(defn dissoc! [c k]
  (->> c
       (do (del (get c k)))))

(defn update! [c k f #* args #** kwargs]
  (->> c
       (do (setv (get c k) (f (get c k) #* args #** kwargs)))))

(defmacro ap-update! [c k form]
  `(update! ~c ~k (fn [it] ~form)))

(defn assoc-in! [c ks v]
  (->> c
       (do
         (match ks
                #(k) (assoc! c k v)
                #(k #* ks) (let [nc (-get c k)]
                             (when (none? nc)
                               (setv nc (dict))
                               (assoc! c k (dict)))
                             (assoc-in! nc ks v))
                _ (raise IndexError)))))

(defn dissoc-in! [c ks]
  (->> c
       (do
         (match ks
                #(k) (dissoc! c k)
                #(k #* ks) (dissoc-in! (-get c k) ks)
                _ (raise IndexError)))))

(defn update-in! [c ks f #* args #** kwargs]
  (->> c
       (do
         (match ks
                #(k) (update! c k f #* args #** kwargs)
                #(k #* ks) (update-in! (-get c k) ks f #* args #** kwargs)
                _ (raise IndexError)))))

(defmacro ap-update-in! [c ks form]
  `(update-in! ~c ~ks (fn [it] ~form)))


;;; conj

(defn [singledispatch] into [c iterable]
  (reduce conj c iterable))

(defn [singledispatch] conj [c x]
  (raise TypeError))

(conj.register
  (fn [#^ seq c x]
    (seq (cons x c))))


;;; conj!

(defn [singledispatch] into! [c iterable]
  (reduce conj! c iterable))

(into!.register
  (fn [#^ MutableSequence c iterable]
    (->> c
         (do (.extend c iterable)))))

(into!.register
  (fn [#^ (| MutableMapping MutableSet) c iterable]
    (->> c
         (do (.update c iterable)))))

(defn [singledispatch] conj! [c x]
  (raise TypeError))

(conj!.register
  (fn [#^ MutableSequence c x]
    (->> c
         (do (.append c x)))))

(conj!.register
  (fn [#^ MutableMapping c x]
    (let [#(k v) x] (assoc! c k v))))

(conj!.register
  (fn [#^ MutableSet c x]
    (->> c
         (do (.add c x)))))

(defn [singledispatch] disj! [c x]
  (raise TypeError))

(disj!.register
  (fn [#^ MutableSet c x]
    (->> c
         (do (.discard c x)))))


;;; peek

(defn [singledispatch] peek [c]
  (raise TypeError))

(peek.register
  (fn [#^ seq c]
    (first c)))

(peek.register
  (fn [#^ Sequence c]
    (-get c -1)))

(defn [singledispatch] pop [c]
  (raise TypeError))

(pop.register
  (fn [#^ seq c]
    (rest c)))

(defn [singledispatch] pop! [c]
  (raise TypeError))

(pop!.register
  (fn [#^ MutableSequence c]
    (->> c
         (do (.pop c)))))


;;; dicttools

(defn [singledispatch] items [m]
  (raise TypeError))

(items.register
  (fn [#^ Sequence m]
    (enumerate m)))

(items.register
  (fn [#^ Mapping m]
    (.items m)))

(items.register
  (fn [#^ Set m]
    (ap-map #(it it) m)))

(defn [singledispatch] keys [m]
  (raise TypeError))

(keys.register
  (fn [#^ Sequence m]
    (range (len m))))

(keys.register
  (fn [#^ Mapping m]
    (.keys m)))

(keys.register
  (fn [#^ Set m]
    m))

(defn [singledispatch] vals [m]
  (raise TypeError))

(vals.register
  (fn [#^ (| Sequence Set) m]
    m))

(vals.register
  (fn [#^ Mapping m]
    (.values m)))

(defn merge [#* ms]
  (ap-reduce
    (let [#(k v) it]
      (assoc! acc k v))
    (dict) (mapcat items ms)))

(defn merge-with [f #* ms]
  (ap-reduce
    (let [#(k v) it]
      (if (contains? acc k)
          (update! acc k f v)
          (assoc! acc k v)))
    (dict) (mapcat items ms)))

(defmacro ap-merge-with [form #* ms]
  `(merge-with (fn [acc it] ~form) ~@ms))



(export
  :objects [contains? -get -get-in -cut -len -reversed
            assoc! dissoc! update! assoc-in! dissoc-in! update-in!
            into conj into! conj! disj! peek pop pop!
            items keys vals merge merge-with]
  :macros [ap-update! ap-update-in! ap-merge-with])
