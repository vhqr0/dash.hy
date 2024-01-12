(require
  dash.dash.polyfill *)

(eval-and-compile
  (import
    dash.dash.polyfill *
    dash.dash.cons *))

(import
  collections [defaultdict])


;; threading macros

(eval-and-compile
  (defn -thread-first-form [x form]
    (if (sexp? form) (let [#(car #* forms) form] `(~car ~x ~@forms)) `(~form ~x)))
  (defn -thread-last-form [x form]
    (if (sexp? form) `(~@form ~x) `(~form ~x))))

(defmacro -> [x #* body]
  (loop [s (seq body) acc x]
        (if (empty? s) acc (recur (rest s) (-thread-first-form acc (first s))))))

(defmacro ->> [x #* body]
  (loop [s (seq body) acc x]
        (if (empty? s) acc (recur (rest s) (-thread-last-form acc (first s))))))

(defmacro as-> [x name #* body]
  `(let [~name ~x]
     ~@(loop [s (seq body)]
             (unless (empty? s)
               (yield `(setv ~name ~(first s)))
               (recur (rest s))))
     ~name))

(defmacro doto [x #* body]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(loop [s (seq body)]
               (unless (empty? s)
                 (yield `(-> ~$ ~(first s)))
                 (recur (rest s))))
       ~$)))

(defmacro some-> [x #* body]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(loop [s (seq body)]
               (unless (empty? s)
                 (yield `(unless (none? ~$) (setv ~$ (-> ~$ ~(first s)))))
                 (recur (rest s))))
       ~$)))

(defmacro some->> [x #* body]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(loop [s (seq body)]
               (unless (empty? s)
                 (yield `(unless (none? ~$) (setv ~$ (->> ~$ ~(first s)))))
                 (recur (rest s))))
       ~$)))

(defmacro cond-> [x #* clauses]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(loop [s (seq clauses)]
               (unless (empty? s)
                 (yield `(when ~(first s) (setv ~$ (-> ~$ ~(first (rest s))))))
                 (recur (rest (rest s)))))
       ~$)))

(defmacro cond->> [x #* clauses]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(loop [s (seq clauses)]
               (unless (empty? s)
                 (yield `(when ~(first s) (setv ~$ (->> ~$ ~(first (rest s))))))
                 (recur (rest (rest s)))))
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

(defn -last [iterable]
  (loop [s (seq iterable)]
        (cond (empty? s) None
              (empty? (rest s)) (first s)
              True (recur (rest s)))))

(defn -butlast [iterable]
  (loop [s (seq iterable)]
        (when (and (not (empty? s)) (not (empty? (rest s))))
          (yield (first s))
          (recur (rest s)))))

(defn -take-last [n iterable] (assert (>= n 0)) (-last (-sized-loose-window n iterable)))
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


;; iter op

(defn -count [iterable]
  (if (countable? iterable)
      (len iterable)
      (--reduce-from (inc acc) 0 iterable)))

(defn -nth [iterable n]
  (assert (>= n 0))
  (if (sequence? iterable)
      (-getitem iterable n)
      (first (-nthrest iterable n))))

(defn -nthrest [iterable n]
  (assert (>= n 0))
  (loop [s (seq iterable) n n]
        (cond (empty? s) (raise IndexError)
              (<= n 0) s
              True (recur (rest s) (dec n)))))


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

(defn -should-flatten? [o]
  (and (not (str? o)) (not (bytes? o)) (iterable? o)))

(defn -flatten [iterable]
  (loop [s (seq iterable)]
        (unless (empty? s)
          (if (-should-flatten? (first s))
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

(defn -notfn [pred] (fn [o] (not (pred o))))
(defn -orfn [#* preds] (fn [o] (any (--map (it o) preds))))
(defn -andfn [#* preds] (fn [o] (all (--map (it o) preds))))

(defn -comp [#* fns]
  (if fns
      (let [#(#* trans f) fns]
        (if trans
            (let [acc (--reduce (fn [o] (acc (it o))) trans)]
              (fn [#* args #** kwargs] (acc (f #* args #** kwargs))))
            f))
      identity))

(defn -juxtv [#* fns] (fn [o] (tuple (--map (it o) fns))))
(defn -juxtkw [#** fns] (fn [o] (-map-vals fns (fn [it] (it o)))))

(defmacro --juxtv [#* forms] `(-juxtv ~@(--map `(fn [it] ~it) forms)))
(defmacro --juxtkw [#* clauses]
  `(-juxtkw ~@(--map-indexed (if (even? it-index) it `(fn [it] ~it)) clauses)))


;; get/set/del/update item

;; like getattr/setattr/delattr
(defn -getitem [o k] (get o k))
(defn -setitem [o k v] (setv (get o k) v))
(defn -delitem [o k] (del (get o k)))
(defn -updateitem [o k f #* args #** kwargs]
  (setv (get o k) (f (get o k) #* args #** kwargs)))

(defn -getitem-in [o ks]
  (-reduce-from -getitem o ks))
(defn -setitem-in [o ks v]
  (let [#(#* ks k) ks] (-setitem (-getitem-in o ks) k v)))
(defn -delitem-in [o ks]
  (let [#(#* ks k) ks] (-delitem (-getitem-in o ks) k)))
(defn -updateitem-in [o ks f #* args #** kwargs]
  (let [#(#* ks k) ks] (-updateitem (-getitem-in o ks) k f #* args #** kwargs)))

(defn -assoc! [o k v] (doto o (-setitem k v)))
(defn -dissoc! [o k] (doto o (-delitem k)))
(defn -update! [o k f #* args #** kwargs] (doto o (-updateitem k f #* args #** kwargs)))

(defn -assoc-in! [o ks v] (doto o (-setitem-in ks v)))
(defn -dissoc-in! [o ks] (doto o (-delitem-in ks)))
(defn -update-in! [o ks f #* args #** kwargs] (doto o (-updateitem-in ks f #* args #** kwargs)))

(defn -assoc [o k v] (-assoc! (.copy o) k v))
(defn -dissoc [o k] (-dissoc! (.copy o) k))
(defn -update [o k f #* args #** kwargs] (-update! (.copy o) k f #* args #** kwargs))

(defn -assoc-in [o ks v]
  (let [#(k #* ks) ks]
    (if ks
        (-update o k -assoc-in ks v)
        (-assoc o k v))))

(defn -dissoc-in [o ks]
  (let [#(k #* ks) ks]
    (if ks
        (-update o k -dissoc-in ks)
        (-dissoc o k))))

(defn -update-in [o ks f #* args #** kwargs]
  (let [#(k #* ks) ks]
    (if ks
        (-update o k -update-in ks f #* args #** kwargs)
        (-update o k f #* args #** kwargs))))

(defmacro --udpateitem [o k form] `(-updateitem ~o ~k (fn [it] ~form)))
(defmacro --updateitem-in [o ks form] `(-updateitem-in ~o ~ks (fn [it] ~form)))
(defmacro --update! [o k form] `(-update! ~o ~k (fn [it] ~form)))
(defmacro --update-in! [o ks form] `(-update-in! ~o ~ks (fn [it] ~form)))
(defmacro --update [o k form] `(-update ~o ~k (fn [it] ~form)))
(defmacro --update-in [o ks form] `(-update-in ~o ~ks (fn [it] ~form)))


;; dict op

(defn -items [o]
  (cond (map? o) (.items o)
        (set? o) (--map #(it it) o)
        (sequence? o) (enumerate o)
        True (raise TypeError)))

(defn -keys [o]
  (cond (map? o) (.keys o)
        (set? o) o
        (sequence? o) (range (len o))
        True (raise TypeError)))

(defn -vals [o]
  (cond (map? o) (.values o)
        (or (set? o) (sequence? o)) o
        True (raise TypeError)))

(defn -reduce-items [o f init] (--reduce-from (let [#(k v) it] (f acc k v)) init (-items o)))

(defn -map-items [o f] (dict (--map (let [#(k v) it] (f k v))    (-items o))))
(defn -map-keys  [o f] (dict (--map (let [#(k v) it] #((f k) v)) (-items o))))
(defn -map-vals  [o f] (dict (--map (let [#(k v) it] #(k (f v))) (-items o))))

(defn -filter-items [o pred] (dict (--filter (let [#(k v) it] (pred k v)) (-items o))))
(defn -filter-keys  [o pred] (dict (--filter (let [#(k v) it] (pred k))   (-items o))))
(defn -filter-vals  [o pred] (dict (--filter (let [#(k v) it] (pred v))   (-items o))))

(defn -select-keys [o ks] (dict (--map #(it (-get o it)) ks)))

(defn -merge [#* os]
  (--reduce-from
    (let [#(k v) it] (-assoc! acc k v))
    (dict)
    (-concat-in (-map -items os))))

(defn -merge-with [f #* os]
  (--reduce-from
    (let [#(k v) it] (if (-contains? acc k) (-update! acc k f v) (-assoc! acc k v)))
    (dict)
    (-concat-in (-map -items os))))

(defmacro --reduce-items [o form init] `(-reduce-items ~o (fn [acc k v] ~form) ~init))
(defmacro --map-items [o form] `(-map-items ~o (fn [k v] ~form)))
(defmacro --map-keys [o form] `(-map-keys ~o (fn [it] ~form)))
(defmacro --map-vals [o form] `(-map-vals ~o (fn [it] ~form)))
(defmacro --filter-items [o form] `(-filter-items ~o (fn [k v] ~form)))
(defmacro --filter-keys [o form] `(-filter-keys ~o (fn [it] ~form)))
(defmacro --filter-vals [o form] `(-filter-vals ~o (fn [it] ~form)))
(defmacro --merge-with [form #* os] `(-merge-with (fn [acc it] ~form) ~@os))


;; set op

(defn -union        [#* os] (if os (let [#(o #* os) os] (.union        o #* os)) (set)))
(defn -difference   [#* os] (if os (let [#(o #* os) os] (.difference   o #* os)) (set)))
(defn -intersection [#* os] (if os (let [#(o #* os) os] (.intersection o #* os)) (set)))

(defn -symmetric-difference [o1 o2] (.symmetric-difference o1 o2))

(defn -subset?   [o1 o2] (.issubset   o1 o2))
(defn -superset? [o1 o2] (.issuperset o1 o2))
(defn -disjoint? [o1 o2] (.isdisjoint o1 o2))


;; coll get

(defn -contains? [o k]
  (cond (or (set? o) (map? o)) (in k o)
        (sequence? o) (in k (range (len o)))
        True (raise TypeError)))

(defn -get [o k [d None]]
  (cond (set? o) (if (in k o) k d)
        (map? o) (.get o k d)
        (sequence? o) (if (in k (range (len o))) (-getitem o k) d)
        True (raise TypeError)))

(defn -get-in [o ks [d None]]
  (loop [s (seq ks) acc o]
        (cond (none? acc) d
              (empty? s) acc
              True (recur (rest s) (-get acc (first s))))))

(defn -collfn [o] (fn [k] (-get o k)))
(defn -keyfn [k] (fn [o] (-get o k)))
(defn -juxtv-keyfn [#* ks] (-juxtv #* (-map -keyfn ks)))
(defn -juxtkw-keyfn [#** ks] (-juxtkw #** (-map-vals ks -keyfn)))


;; coll op

(defn -clearitem [o]
  (.clear o))

(defn -extenditem [o iterable]
  (cond (set? o) (.update o iterable)
        (map? o) (.update o iterable)
        (sequence? o) (.extend o iterable)
        True (raise TypeError)))

(defn -additem [o x]
  (cond (set? o) (.add o x)
        (map? o) (let [#(k v) x] (-setitem o k v))
        (sequence? o) (.append o x)
        True (raise TypeError)))

(defn -removeitem [o x]
  (cond (set? o) (.discard o x)
        True (raise TypeError)))

(defn -popitem [o]
  (cond (set? o) (.pop o)
        (map? o) (.popitem o)
        (sequence? o) (.pop o)
        True (raise TypeError)))

(defn -peekitem [o]
  (cond (sequence? o) (-getitem o -1)
        True (raise TypeError)))

(defn -empty! [o] (doto o (-clearitem)))
(defn -into! [o iterable] (doto o (-extenditem iterable)))
(defn -conj! [o x] (doto o (-additem x)))
(defn -disj! [o x] (doto o (-removeitem x)))
(defn -pop! [o] (doto o (-popitem)))

(defn -empty [o] (.__class__ o))
(defn -into [o iterable]
  (if (seq? o) (seq (conlist-in-reverse iterable :last o)) (-into! (.copy o) iterable)))
(defn -conj [o x] (if (seq? o) (seq (cons x o)) (-conj! (.copy o) x)))
(defn -disj [o x] (-disj! (.copy o) x))
(defn -pop [o] (if (seq? o) (rest o) (-pop! (.copy o))))
(defn -peek [o] (if (seq? o) (first o) (-peekitem o)))



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
            -unsized-window -sized-window -sized-loose-window
            -last -butlast -take-last -drop-last
            -split-at -split-with
            -partition -partition-all -partition-step -partition-all-step -partition-by
            ;; iter op
            -count -nth -nthrest
            ;; iter trans
            -replace -distinct -dedupe -flatten -group-by
            ;; functools
            -argv -argkw -arg -applyv -applykw -apply -funcall -trampoline
            -partial -rpartial -notfn -andfn -orfn -comp -juxtv -juxtkw
            ;; get/set/del/update item
            -getitem -setitem -delitem -updateitem
            -getitem-in -setitem-in -delitem-in -updateitem-in
            -assoc! -dissoc! -update! -assoc-in! -dissoc-in! -update-in!
            -assoc -dissoc -update -assoc-in -dissoc-in -update-in
            ;; dict op
            -items -keys -vals
            -reduce-items
            -map-items -map-keys -map-vals
            -filter-items -filter-keys -filter-vals
            -select-keys -merge -merge-with
            ;; set op
            -union -difference -intersection -symmetric-difference
            -subset? -superset? -disjoint?
            ;; coll get
            -contains? -get -get-in
            -collfn -keyfn -juxtv-keyfn -juxtkw-keyfn
            ;; coll op
            -clearitem -extenditem -additem -removeitem -popitem -peekitem
            -empty! -into! -conj! -disj! -pop! -empty -into -conj -disj -pop -peek
            ]
  :macros [
           ;; threading macros
           -> ->> as-> doto some-> some->> cond-> cond->>
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
           ;; get/set/del/update item
           --updateitem --updateitem-in --update! --update-in! --update --update-in
           ;; dict op
           --reduce-items
           --map-items --map-keys --map-vals
           --filter-items --filter-keys --filter-vals
           --merge-with
           ])
