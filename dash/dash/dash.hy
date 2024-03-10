(require
  dash.dash.polyfill *)

(import
  dash.dash.polyfill *
  dash.dash.cons *
  functools [singledispatch]
  collections.abc [Sequence Mapping Set MutableSequence MutableMapping MutableSet]
  collections [defaultdict])


;; threading macros

(defmacro -> [x #* body]
  (-reduce-from
    (fn [acc it]
      (match it
             #(x #* xs) `(~x ~acc ~@xs)
             _ `(~it ~acc)))
    x body))

(defmacro ->> [x #* body]
  (-reduce-from
    (fn [acc it]
      (match it
             #(#* xs) `(~@xs ~acc)
             _ `(~it ~acc)))
    x body))

(defmacro as-> [x name #* body]
  (-reduce-from
    (fn [acc it]
      `(let [~name ~acc] ~it))
    x body))

(defmacro doto [x #* body]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(-map (fn [it] `(-> ~$ ~it)) body)
       ~$)))


;; reduce

(defmacro --each [iterable #* body]
  `(let [it None] (for [it ~iterable] ~@body)))

(defmacro --each-indexed [iterable #* body]
  `(let [#(it-index it) #(None None)]
     (for [#(it-index it) (enumerate ~iterable)]
       ~@body)))

(defmacro --dotimes [n #* body]
  `(--each (range ~n) ~@body))

(defmacro --reduce-from [form init iterable]
  `(let [acc ~init]
     (--each ~iterable (setv acc ~form))
     acc))

(defmacro --reductions-from [form init iterable]
  `(let [acc ~init]
     (yield acc)
     (--each ~iterable (do (setv acc ~form) (yield acc)))))

(defn -each [iterable f] (--each iterable (f it)))
(defn -each-indexed [iterable f] (--each-indexed iterable (f it-index it)))
(defn -dotimes [n f] (assert (>= n 0)) (--dotimes n (f it)))

(defn -reduce-from [f init iterable] (--reduce-from (f acc it) init iterable))
(defn -reductions-from [f init iterable] (--reductions-from (f acc it) init iterable))
(defn -reduce [f iterable] (let [i (iter iterable)] (-reduce-from f (next i) i)))
(defn -reductions [f iterable] (let [i (iter iterable)] (-reductions-from f (next i) i)))

(defmacro --reduce [form iterable] `(-reduce (fn [acc it] ~form) ~iterable))
(defmacro --reductions [form iterable] `(-reductions (fn [acc it] ~form) ~iterable))


;; map filter

(defn -map [f iterable] (--each iterable (yield (f it))))
(defn -map-indexed [f iterable] (--each-indexed iterable (yield (f it-index it))))
(defn -map-unzipped [f iterable] (--each iterable (yield (f #* it))))

(defn -filter [pred iterable] (--each iterable (when (pred it) (yield it))))
(defn -remove [pred iterable] (--each iterable (unless (pred it) (yield it))))

(defn -some [f iterable]
  (loop [s (seq iterable)]
        (unless (empty? s)
          (if-let [it (f (first s))] it (recur (rest s))))))

(defn -every [f iterable]
  (loop [s (seq iterable)]
        (unless (empty? s)
          (when-let [it (f (first s))]
            (if (empty? (rest s)) it (recur (rest s)))))))

(defn -any? [pred iterable] (not (-not-any? pred iterable)))
(defn -all? [pred iterable] (not (-not-all? pred iterable)))
(defn -not-any? [pred iterable] (none? (-some pred iterable)))
(defn -not-all? [pred iterable] (none? (-every pred iterable)))

(defn -mapcat [f iterable] (-concat-in (-map f iterable)))
(defn -mapcat-indexed [f iterable] (-concat-in (-map-indexed f iterable)))

(defn -mapcons [f iterable] (-map (fn [it] (cons (f it) it)) iterable))
(defn -mapcons-indexed [f iterable]
  (-map-indexed (fn [it-index it] (cons (f it-index it) it)) iterable))

(defn -keep [f iterable] (-remove none? (-map f iterable)))
(defn -keep-indexed [f iterable] (-remove none? (-map-indexed f iterable)))

(defmacro --map [form iterable] `(-map (fn [it] ~form) ~iterable))
(defmacro --map-indexed [form iterable] `(-map-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --map-unzipped [form iterable] `(-map-unzipped (fn [#* them] ~form) ~iterable))
(defmacro --filter [form iterable] `(-filter (fn [it] ~form) ~iterable))
(defmacro --remove [form iterable] `(-remove (fn [it] ~form) ~iterable))
(defmacro --some [form iterable] `(-some (fn [it] ~form) ~iterable))
(defmacro --every [form iterable] `(-every (fn [it] ~form) ~iterable))
(defmacro --any? [form iterable] `(-any? (fn [it] ~form) ~iterable))
(defmacro --all? [form iterable] `(-all? (fn [it] ~form) ~iterable))
(defmacro --not-any? [form iterable] `(-not-any? (fn [it] ~form) ~iterable))
(defmacro --not-all? [form iterable] `(-not-all? (fn [it] ~form) ~iterable))
(defmacro --mapcat [form iterable] `(-mapcat (fn [it] ~form) ~iterable))
(defmacro --mapcat-indexed [form iterable] `(-mapcat-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --mapcons [form iterable] `(-mapcons (fn [it] ~form) ~iterable))
(defmacro --mapcons-indexed [form iterable] `(-mapcons-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --keep [form iterable] `(-keep (fn [it] ~form) ~iterable))
(defmacro --keep-indexed [form iterable] `(-keep-indexed (fn [it-index it] ~form) ~iterable))


;; iter gen

(defn -iterate [f init]
  (loop [acc init] (do (yield acc) (recur (f acc)))))
(defn -iterate-n [n f init]
  (assert (>= n 0))
  (loop [acc init n n] (when (>= n 1) (yield acc) (recur (f acc) (dec n)))))
(defn -range [] (-iterate inc 0))

(defn -repeat [o] (loop [] (do (yield o) (recur))))
(defn -repeat-n [n o] (assert (>= n 0)) (--dotimes n (yield o)))
(defn -repeatedly [f] (loop [] (do (yield (f)) (recur))))
(defn -repeatedly-n [n f] (assert (>= n 0)) (--dotimes n (yield (f))))
(defn -cycle [iterable] (-concat-in (-repeat (seq iterable))))
(defn -cycle-n [n iterable] (assert (>= n 0)) (-concat-in (-repeat-n n (seq iterable))))

(defmacro --iterate [form init] `(-iterate (fn [it] ~form) ~init))
(defmacro --iterate-n [n form init] `(-iterate-n ~n (fn [it] ~form) ~init))
(defmacro --repeatedly [form] `(-repeatedly (fn [] ~form)))
(defmacro --repeatedly-n [n form] `(-repeatedly-n ~n (fn [] ~form)))


;; iter mux

(defn -concat-in [iterables] (--each iterables (yield-from it)))
(defn -concat [#* iterables] (-concat-in iterables))

(defn -zip-in [iterables]
  (loop [ss (list (-map seq iterables))]
        (when (and ss (not (-any? empty? ss)))
          (yield (list (-map first ss)))
          (recur (list (-map rest ss))))))

(defn -zip [#* iterables] (-zip-in iterables))

(defn -zip-fill-in [fill-val iterables]
  (loop [ss (list (-map seq iterables))]
        (when (and ss (not (-all? empty? ss)))
          (yield (list (--map (if (empty? it) fill-val (first it)) ss)))
          (recur (list (--map (if (empty? it) it (rest it)) ss))))))

(defn -zip-fill [fill-val #* iterables] (-zip-fill-in fill-val iterables))

(defn -tee [iterable] (let [s (seq iterable)] (--repeatedly (iter s))))
(defn -tee-n [n iterable] (assert (>= n 0)) (let [s (seq iterable)] (--repeatedly-n n (iter s))))

(defn -interleave-in [iterables] (-concat-in (-zip-in iterables)))
(defn -interleave [#* iterables] (-interleave-in iterables))
(defn -interleave-fill-in [fill-val iterables] (-concat-in (-zip-fill-in fill-val iterables)))
(defn -interleave-fill [fill-val #* iterables] (-interleave-fill-in fill-val iterables))
(defn -interpose [sep iterable] (-drop 1 (-interleave (-repeat sep) iterable)))


;; iter part

(defn -take [n iterable]
  (assert (>= n 0))
  (loop [s (seq iterable) n n]
        (unless (or (<= n 0) (empty? s)) (yield (first s)) (recur (rest s) (dec n)))))

(defn -drop [n iterable]
  (assert (>= n 0))
  (loop [s (seq iterable) n n]
        (if (or (<= n 0) (empty? s)) s (recur (rest s) (dec n)))))

(defn -take-while [pred iterable]
  (loop [s (seq iterable)]
        (when (and (not (empty? s)) (pred (first s))) (yield (first s)) (recur (rest s)))))

(defn -drop-while [pred iterable]
  (loop [s (seq iterable)]
        (if (and (not (empty? s)) (pred (first s))) (recur (rest s)) s)))

(defn -take-nth [n iterable]
  (assert (>= n 1))
  (loop [s (seq iterable) i n]
        (cond (empty? s) None
              (= i n) (do (yield (first s)) (recur (rest s) 1))
              True (recur (rest s) (inc i)))))

(defn -drop-nth [n iterable]
  (assert (>= n 1))
  (loop [s (seq iterable) i n]
        (cond (empty? s) None
              (= i n) (recur (rest s) 1)
              True (do (yield (first s)) (recur (rest s) (inc i))))))

(defn -unsized-window [iterable]
  (loop [s (seq iterable)]
        (unless (empty? s) (yield s) (recur (rest s)))))

;;; sized window:
;;
;; - strict: check len >= n, then yield and step
;; - loose: yield, then check len >= n+1 and step

(defn -sized-window [n iterable]
  (assert (>= n 1))
  (loop [s (seq iterable) t (-drop (dec n) s)]
        (unless (empty? t) (yield s) (recur (rest s) (rest t)))))

(defn -sized-loose-window [n iterable]
  (assert (>= n 0))
  (loop [s (seq iterable) t (-drop n s)]
        (do (yield s) (unless (empty? t) (recur (rest s) (rest t))))))

(defn -take-last [n iterable] (assert (>= n 0)) (last (-sized-loose-window n iterable)))
(defn -drop-last [n iterable] (assert (>= n 0)) (-map first (-sized-window (inc n) iterable)))

(defn -split-at [n iterable]
  (assert (>= n 0))
  (loop [s (seq iterable) acc (list) n n]
        (if (or (<= n 0) (empty? s))
            #(acc s)
            (recur (rest s) (-conj! acc (first s)) (dec n)))))

(defn -split-with [pred iterable]
  (loop [s (seq iterable) acc (list)]
        (if (or (empty? s) (not (pred (first s))))
            #(acc s)
            (recur (rest s) (-conj! acc (first s))))))

(defn -partition [n iterable] (assert (>= n 1)) (-partition-step n n iterable))
(defn -partition-all [n iterable] (assert (>= n 1)) (-partition-all-step n n iterable))
(defn -partition-step [n step iterable]
  (assert (and (>= n 1) (>= step 1)))
  (--map (list (-take n it)) (-take-nth step (-sized-window n iterable))))
(defn -partition-all-step [n step iterable]
  (assert (and (>= n 1) (>= step 1)))
  (--map (list (-take n it)) (-take-nth step (-unsized-window iterable))))

(defn -partition-by [f iterable]
  (loop [s (seq (-mapcons f iterable))]
        (unless (empty? s)
          (let [g (car (first s))
                #(acc ns) (-split-with (fn [it] (= (car it) g)) s)]
            (yield (list (-map cdr acc)))
            (recur ns)))))

(defmacro --take-while [form iterable] `(-take-while (fn [it] ~form) ~iterable))
(defmacro --drop-while [form iterable] `(-drop-while (fn [it] ~form) ~iterable))
(defmacro --split-with [form iterable] `(-split-with (fn [it] ~form) ~iterable))
(defmacro --partition-by [form iterable] `(-partition-by (fn [it] ~form) ~iterable))


;; iter misc

(defn -replace [smap iterable] (--map (-get smap it it) iterable))

(defn -distinct [iterable]
  (loop [s (seq iterable) seen (set)]
        (unless (empty? s)
          (let [it (first s)]
            (if (in it seen)
                (recur (rest s) seen)
                (do
                  (yield it)
                  (recur (rest s) (-conj! seen it))))))))

(defn -dedupe [iterable]
  (loop [s (seq iterable) pre None]
        (unless (empty? s)
          (let [it (first s)]
            (unless (= it pre)
              (yield it))
            (recur (rest s) it)))))

(defn -flatten [iterable]
  (loop [s (seq iterable)]
        (unless (empty? s)
          (if (coll? (first s))
              (yield-from (-flatten (first s)))
              (yield (first s)))
          (recur (rest s)))))

(defn -group-by [f iterable]
  (--reduce-from (-update! acc (f it) -conj! it) (defaultdict list) iterable))

(defmacro --group-by [form iterable] `(-group-by (fn [it] ~form) ~iterable))


;; functools

(defn -argv [#* args] args)
(defn -argkw [#** kwargs] kwargs)
(defn -arg [#* args #** kwargs] #(args kwargs))
(defn -applyv [f args] (f #* args))
(defn -applykw [f kwargs] (f #** kwargs))
(defn -apply [f args kwargs] (f #* args #** kwargs))
(defn -funcall [f #* args #** kwargs] (f #* args #** kwargs))

(defn -trampoline [o] (loop [o o] (if (fn? o) (recur (o)) o)))

(defn -partial [f #* largs #** lkwargs]
  (fn [#* rargs #** rkwargs] (f #* largs #* rargs #** lkwargs #** rkwargs)))
(defn -rpartial [f #* rargs #** rkwargs]
  (fn [#* largs #** lkwargs] (f #* largs #* rargs #** lkwargs #** rkwargs)))

(defn -comp [#* fns]
  (match fns
         #() identity
         #(f) f
         #(f1 f2) (fn [#* args #** kwargs] (f1 (f2 #* args #** kwargs)))
         #(f1 f2 #* fns) (-comp (fn [o] (f1 (f2 o))) #* fns)))

(defn -curry [n [f None] #* args]
  (if f
      (fn [#* more-args]
        (let [args #(#* args #* more-args)]
          (if (>= (len args) n) (f #* args) (-curry n f #* args))))
      (fn [f #* args] (-curry n f #* args))))

(defn -notfn [pred] (fn [o] (not (pred o))))
(defn -orfn [#* preds] (fn [o] (any (--map (it o) preds))))
(defn -andfn [#* preds] (fn [o] (all (--map (it o) preds))))

(defn -juxtv [#* fns] (fn [o] (tuple (--map (it o) fns))))
(defn -juxtkw [#** fns] (fn [o] (dict (--map (let [#(k f) it] #(k (f o))) (-items fns)))))

(defmacro --juxtv [#* forms] `(-juxtv ~@(--map `(fn [it] ~it) forms)))
(defmacro --juxtkw [#* clauses]
  `(-juxtkw ~@(--map-indexed (if (even? it-index) it `(fn [it] ~it)) clauses)))


;; coll get assoc

(defn [singledispatch] -contains? [c k]
  (raise TypeError))

(-contains?.register
  (fn [#^ Sequence c k]
    (in k (range (len c)))))

(-contains?.register
  (fn [#^ (| Mapping Set) c k]
    (in k c)))

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
    (if (-contains? c k) k default)))

(defn -assoc! [c k value]
  (with-ignore c
    (setv (get c k) value)))

(defn -dissoc! [c k]
  (with-ignore c
    (del (get c k))))

(defn -update! [c k f #* args #** kwargs]
  (with-ignore c
    (setv (get c k) (f (get c k) #* args #** kwargs))))

(defn -get-in [c ks [default None]]
  (loop [c c ks (seq ks)]
        (if (empty? ks)
            c
            (let [n (-get c (first ks))]
              (if (none? n)
                  default
                  (recur n (rest ks)))))))

(defn -assoc-in! [c ks value]
  (with-ignore c
    (loop [c c ks (seq ks)]
          (cond (empty? ks) (raise ValueError)
                (empty? (rest ks)) (-assoc! c (first ks) value)
                True (let [n (-get c (first ks))]
                       (when (none? n)
                         (setv n (dict))
                         (-assoc! c (first ks) n))
                       (recur n (rest ks)))))))

(defn -dissoc-in! [c ks]
  (with-ignore c
    (loop [c c ks (seq ks)]
          (cond (empty? ks) (raise ValueError)
                (empty? (rest ks)) (-dissoc! c (first ks))
                True (let [n (-get c (first ks))]
                       (when (none? n)
                         (raise KeyError))
                       (recur n (rest ks)))))))

(defn -update-in! [c ks f #* args #** kwargs]
  (with-ignore c
    (loop [c c ks (seq ks)]
          (cond (empty? ks) (raise ValueError)
                (empty? (rest ks)) (-update! c (first ks) f #* args #** kwargs)
                True (let [n (-get c (first ks))]
                       (when (none? n)
                         (raise KeyError))
                       (recur n (rest ks)))))))

(defmacro --update! [c k form] `(-update! ~c ~k (fn [it] ~form)))
(defmacro --update-in! [c ks form] `(-update-in! ~c ~k (fn [it] ~form)))


;; coll into conj

(defn -empty [c]
  (.__class__ c))

(defn -into [c iterable]
  (-reduce-from -conj c iterable))

(defn [singledispatch] -conj [c x]
  (raise TypeError))

(-conj.register
  (fn [#^ seq c x]
    (seq (cons x c))))

(defn [singledispatch] -into! [c iterable]
  (-reduce-from -conj! c iterable))

(-into!.register
  (fn [#^ MutableSequence c iterable]
    (with-ignore c
      (.extend c iterable))))

(-into!.register
  (fn [#^ (| MutableMapping MutableSet) c iterable]
    (with-ignore c
      (.update c iterable))))

(defn [singledispatch] -conj! [c x]
  (raise TypeError))

(-conj!.register
  (fn [#^ MutableSequence c x]
    (with-ignore c
      (.append c x))))

(-conj!.register
  (fn [#^ MutableMapping c x]
    (let [#(k v) x] (-assoc! c k v))))

(-conj!.register
  (fn [#^ MutableSet c x]
    (with-ignore c
      (.add c x))))

(defn [singledispatch] -disj! [c x]
  (raise TypeError))

(-disj!.register
  (fn [#^ MutableSet c x]
    (with-ignore c
      (.discard c x))))


;; coll peek pop

(defn [singledispatch] -peek [c]
  (raise TypeError))

(-peek.register
  (fn [#^ Sequence c]
    (-get c -1)))

(-peek.register
  (fn [#^ seq c]
    (first c)))

(defn [singledispatch] -pop [c]
  (raise TypeError))

(-pop.register
  (fn [#^ seq c]
    (rest c)))

(defn [singledispatch] -pop! [c]
  (raise TypeError))

(-pop!.register
  (fn [#^ MutableSequence c]
    (with-ignore c
      (.pop c))))


;; dicttools

(defn [singledispatch] -items [m]
  (raise TypeError))

(-items.register
  (fn [#^ Sequence m]
    (enumerate m)))

(-items.register
  (fn [#^ Mapping m]
    (.items m)))

(-items.register
  (fn [#^ Set m]
    (--map #(it it) m)))

(defn [singledispatch] -keys [m]
  (raise TypeError))

(-keys.register
  (fn [#^ Sequence m]
    (range (len m))))

(-keys.register
  (fn [#^ Mapping m]
    (.keys m)))

(-keys.register
  (fn [#^ Set m]
    m))

(defn [singledispatch] -vals [m]
  (raise TypeError))

(-vals.register
  (fn [#^ (| Sequence Set) m]
    m))

(-vals.register
  (fn [#^ Mapping m]
    (.values m)))

(defn -reduce-items [m f init] (--reduce-from (let [#(k v) it] (f acc k v)) init (-items m)))
(defn -reduce-keys  [m f init] (--reduce-from (f acc it) init (-keys m)))
(defn -reduce-vals  [m f init] (--reduce-from (f acc it) init (-vals m)))

(defn -map-items [m f] (dict (--map (let [#(k v) it] (f k v))    (-items m))))
(defn -map-keys  [m f] (dict (--map (let [#(k v) it] #((f k) v)) (-items m))))
(defn -map-vals  [m f] (dict (--map (let [#(k v) it] #(k (f v))) (-items m))))

(defn -filter-items [m pred] (dict (--filter (let [#(k v) it] (pred k v)) (-items m))))
(defn -filter-keys  [m pred] (dict (--filter (let [#(k v) it] (pred k))   (-items m))))
(defn -filter-vals  [m pred] (dict (--filter (let [#(k v) it] (pred v))   (-items m))))

(defn -select-keys [m ks [default None]] (dict (--map #(it (-get m it default)) ks)))

(defn -merge [#* ms]
  (--reduce-from
    (let [#(k v) it] (-assoc! acc k v))
    (dict)
    (-concat-in (-map -items ms))))

(defn -merge-with [f #* ms]
  (--reduce-from
    (let [#(k v) it] (if (-contains? acc k) (-update! acc k f v) (-assoc! acc k v)))
    (dict)
    (-concat-in (-map -items ms))))

(defmacro --reduce-items [m form init] `(-reduce-items ~m (fn [acc k v] ~form) ~init))
(defmacro --reduce-keys [m form init] `(-reduce-keys ~m (fn [acc k] ~form) ~init))
(defmacro --reduce-vals [m form init] `(-reduce-vals ~m (fn [acc v] ~form) ~init))
(defmacro --map-items [m form] `(-map-items ~m (fn [k v] ~form)))
(defmacro --map-keys [m form] `(-map-keys ~m (fn [k] ~form)))
(defmacro --map-vals [m form] `(-map-vals ~m (fn [v] ~form)))
(defmacro --filter-items [m form] `(-filter-items ~m (fn [k v] ~form)))
(defmacro --filter-keys [m form] `(-filter-keys ~m (fn [k] ~form)))
(defmacro --filter-vals [m form] `(-filter-vals ~m (fn [v] ~form)))
(defmacro --merge-with [form #* ms] `(-merge-with (fn [acc it] ~form) ~@ms))


;; settools

(defn -union        [#* ss] (if ss (let [#(s #* ss) ss] (.union        s #* ss)) (set)))
(defn -difference   [#* ss] (if ss (let [#(s #* ss) ss] (.difference   s #* ss)) (set)))
(defn -intersection [#* ss] (if ss (let [#(s #* ss) ss] (.intersection s #* ss)) (set)))

(defn -symmetric-difference [s1 s2] (.symmetric-difference s1 s2))

(defn -subset?   [s1 s2] (.issubset   s1 s2))
(defn -superset? [s1 s2] (.issuperset s1 s2))
(defn -disjoint? [s1 s2] (.isdisjoint s1 s2))



(export
  :objects [
            ;; reduce
            -each -each-indexed -dotimes
            -reduce-from -reductions-from -reduce -reductions
            ;; map filter
            -map -map-indexed -map-unzipped
            -filter -remove
            -some -every -any? -all? -not-any? -not-all?
            -mapcat -mapcat-indexed -mapcons -mapcons-indexed
            -keep -keep-indexed
            ;; iter gen
            -iterate -iterate-n -range
            -repeat -repeat-n -repeatedly -repeatedly-n -cycle -cycle-n
            ;; iter mux
            -concat-in -concat -zip-in -zip -zip-fill-in -zip-fill -tee -tee-n
            -interleave-in -interleave -interleave-fill-in -interleave-fill -interpose
            ;; iter part
            -take -drop -take-while -drop-while -take-nth -drop-nth
            -unsized-window -sized-window -sized-loose-window -take-last -drop-last
            -split-at -split-with -partition -partition-all -partition-step -partition-all-step -partition-by
            ;; iter trans
            -replace -distinct -dedupe -flatten -group-by
            ;; functools
            -argv -argkw -arg -applyv -applykw -apply -funcall -trampoline
            -partial -rpartial -comp -curry -notfn -andfn -orfn -juxtv -juxtkw
            ;; coll get assoc
            -contains? -get -assoc! -dissoc! -update! -get-in -assoc-in! -dissoc-in! -update-in!
            ;; coll into conj
            -empty -into -conj -into! -conj! -disj!
            ;; coll peek pop
            -peek -pop -pop!
            ;; dicttools
            -items -keys -vals
            -reduce-items -reduce-keys -reduce-vals
            -map-items -map-keys -map-vals
            -filter-items -filter-keys -filter-vals
            -select-keys -merge -merge-with
            ;; settools
            -union -difference -intersection -symmetric-difference
            -subset? -superset? -disjoint?
            ]
  :macros [
           ;; threading macros
           -> ->> as-> doto
           ;; reduce
           --each --each-indexed --dotimes
           --reduce-from --reductions-from --reduce --reductions
           ;; map filter
           --map --map-indexed --map-unzipped
           --filter --remove
           --some --every --any? --all? --not-any? --not-all?
           --mapcat --mapcat-indexed --mapcons --mapcons-indexed
           --keep --keep-indexed
           ;; iter gen
           --iterate --iterate-n --repeatedly --repeatedly-n
           ;; iter part
           --take-while --drop-while --split-with --partition-by
           ;; iter misc
           --group-by
           ;; functools
           --juxtv --juxtkw
           ;; coll get assoc
           --update! --update-in!
           ;; dicttools
           --reduce-items --reduce-keys --reduce-vals
           --map-items --map-keys --map-vals
           --filter-items --filter-keys --filter-vals
           --merge-with
           ])
