(require
  dash.dash.polyfill *)

(eval-and-compile
  (import
    dash.dash.polyfill *
    dash.dash.sequence *))

(import
  collections [defaultdict])


;; threading macros

(eval-and-compile
  (defn -thread-first-form [x form]
    (if (sexp? form) (let [#(h #* ts) form] `(~h ~x ~@ts)) `(~form ~x)))
  (defn -thread-last-form [x form]
    (if (sexp? form) `(~@form ~x) `(~form ~x))))

(defmacro -> [x #* body]
  (loop [s (seq body) acc x]
        (if (empty? s) acc (recur (rest s) (-thread-first-form x (first s))))))

(defmacro ->> [x #* body]
  (loop [s (seq body) acc x]
        (if (empty? s) acc (recur (rest s) (-thread-last-form x (first s))))))

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


;; let macros

(defmacro --if-let [test then else] `(let [it ~test] (if it ~then ~else)))
(defmacro -if-let [binding then else]
  (let [#(name val) binding] `(let [~name ~val] (if ~name ~then ~else))))
(defmacro --when-let [test #* body] `(let [it ~test] (when it ~@body)))
(defmacro -when-let [binding #* body]
  (let [#(name val) binding] `(let [~name ~val] (when ~name ~@body))))


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
(defn -dotimes [n f] (--dotimes n (f it)))

(defn -reduce-from [f init iterable] (--reduce-from (f acc it) init iterable))
(defn -reductions-from [f init iterable] (--reductions-from (f acc it) init iterable))
(defn -reduce [f iterable] (let [i (iter iterable)] (-reduce-from f (next i) i)))
(defn -reductions [f iterable] (let [i (iter iterable)] (-reductions-from f (next i) i)))

(defmacro --reduce [form iterable] `(-reduce (fn [acc it] ~form) ~iterable))
(defmacro --reductions [form iterable] `(-reductions (fn [acc it] ~form) ~iterable))


;; map filter

(defn -map* [f iterable] (--each iterable (yield (f #* it))))
(defn -map [f iterable] (--each iterable (yield (f it))))
(defn -map-indexed [f iterable] (--each-indexed iterable (yield (f it-index it))))
(defn -filter [pred iterable] (--each iterable (when (pred it) (yield it))))
(defn -remove [pred iterable] (--each iterable (unless (pred it) (yield it))))
(defn -mapcat [f iterable] (-concat-in (-map f iterable)))
(defn -mapcat-indexed [f iterable] (-concat-in (-map-indexed f iterable)))
(defn -keep [f iterable] (-remove none? (-map f iterable)))
(defn -keep-indexed [f iterable] (-remove none? (-map-indexed f iterable)))
(defn -annotate [f iterable] (--each iterable (yield #((f it) it))))
(defn -annotate-indexed [f iterable] (--each-indexed iterable (yield #((f it-index it) it))))
(defn -some [f iterable] (--each iterable (let [r (f it)] (when r (return r)))) (return None))
(defn -any? [pred iterable] (not (none? (-some pred iterable))))
(defn -all? [pred iterable] (none? (-some (-complement pred) iterable)))

(defmacro --map [form iterable] `(-map (fn [it] ~form) ~iterable))
(defmacro --map-indexed [form iterable] `(-map-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --filter [form iterable] `(-filter (fn [it] ~form) ~iterable))
(defmacro --remove [form iterable] `(-remove (fn [it] ~form) ~iterable))
(defmacro --mapcat [form iterable] `(-mapcat (fn [it] ~form) ~iterable))
(defmacro --mapcat-indexed [form iterable] `(-mapcat-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --keep [form iterable] `(-keep (fn [it] ~form) ~iterable))
(defmacro --keep-indexed [form iterable] `(-keep-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --annotate [form iterable] `(-annotate (fn [it] ~form) ~iterable))
(defmacro --annotate-indexed [form iterable] `(-annotate-indexed (fn [it-index it] ~form) ~iterable))
(defmacro --some [form iterable] `(-some (fn [it] ~form) ~iterable))
(defmacro --any? [form iterable] `(-any? (fn [it] ~form) ~iterable))
(defmacro --all? [form iterable] `(-all? (fn [it] ~form it) ~iterable))


;; iter op

(defn -concat-in [iterables] (--each iterables (yield-from it)))
(defn -concat [#* iterables] (-concat-in iterables))

;; like cons/pair/empty?/first/rest, but for iterable.
;; orig iter should not be used, use iter returned from -pair/-rest.
;; should always check None value, None is seqable, but is not iterable.

(defn -cons [o iterable] (yield o) (yield-from iterable))

(defn -pair   [iterable] (let [it (iter iterable)] (try #((next it) it) (except [StopIteration]))))
(defn -empty? [iterable] (none? (-pair iterable)))
(defn -first  [iterable] (--when-let (-pair iterable) (get it 0)))
(defn -rest   [iterable] (--when-let (-pair iterable) (get it 1)))


;; iter gen

(defn -iterate [f init] (while True (yield init) (setv init (f init))))
(defn -iterate-n [n f init]
  ;;; avoid additional calculations
  ;; (--dotimes n (yield init) (setv init (f init)))
  (when (>= n 1) (yield init) (--dotimes (dec n) (setv init (f init)) (yield init))))

(defn -repeat [o] (while True (yield o)))
(defn -repeat-n [n o] (--dotimes n (yield o)))
(defn -repeatedly [f] (while True (yield (f))))
(defn -repeatedly-n [n f] (--dotimes n (yield (f))))
(defn -cycle [iterable] (-concat-in (-repeat (seq iterable))))
(defn -cycle-n [n iterable] (-concat-in (-repeat-n (seq iterable))))
(defn -range [] (-iterate inc 0))

(defmacro --iterate [form init] `(-iterate (fn [it] ~form) ~init))
(defmacro --iterate-n [n form init] `(-iterate-n ~n (fn [it] ~form) ~init))
(defmacro --repeatedly [form] `(-repeatedly (fn [] ~form)))
(defmacro --repeatedly-n [n form] `(-repeatedly-n ~n (fn [] ~form)))


;; iter mux

(defn -zip-in [iterables]
  (-when-let [ss (list (-map seq iterables))]
    (while (-all? (-complement empty?) ss)
      (yield (list (--map-indexed
                     (do
                       (setv (get ss it-index) (rest it))
                       (first it))
                     ss))))))

(defn -zip [#* iterables] (-zip-in iterables))

(defn -zip-fill-in [fill-val iterables]
  (-when-let [ss (list (-map seq iterables))]
    (while (-any? (-complement empty?) ss)
      (yield (list (--map-indexed
                     (if (empty? it)
                         fill-val
                         (do
                           (setv (get ss it-index) (rest it))
                           (first it)))
                     ss))))))

(defn -zip-fill [fill-val #* iterables] (-zip-fill-in fill-val iterables))

(defn -tee [iterable] (let [s (seq iterable)] (--repeatedly (iter s))))
(defn -tee-n [n iterable] (let [s (seq iterable)] (--repeatedly-n n (iter s))))

(defn -interleave-in [iterables] (-concat-in (-zip-in iterables)))
(defn -interleave [#* iterables] (-interleave-in iterables))
(defn -interleave-fill-in [fill-val iterables] (-concat (-zip-fill-in fill-val iterables)))
(defn -interleave-fill [fill-val #* iterables] (-interleave-fill-in fill-val iterables))
(defn -interpose [sep iterable] (-drop 1 (-interleave (-repeat sep) iterable)))


;; iter part

(defn -nth [iterable n]
  (if (sequence? iterable)
      (get iterable n)
      (first (-nthrest iterable n))))

(defn -nthrest [iterable n] (--if-let (-drop n iterable) it (raise IndexError)))

(defn -take [n iterable]
  (loop [s (seq iterable) n n]
        (unless (or (<= n 0) (empty? s)) (yield (first s)) (recur (rest s) (dec n)))))

(defn -drop [n iterable]
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

(defn -sized-window [n iterable]
  (assert (>= n 1))
  (loop [s (seq iterable) t (-drop (dec n) s)]
        (unless (empty? t) (yield s) (recur (rest s) (rest t)))))

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

(defn -take-last [n iterable] (-last (-sized-window n iterable)))

(defn -drop-last [n iterable] (-map first (-sized-window (inc n) iterable)))

(defn -split-at [n iterable]
  (loop [s (seq iterable) acc (list) n n]
        (if (or (<= n 0) (empty? s))
            #(acc s)
            (recur (rest s) (-conj! acc (first s)) (dec n)))))

(defn -split-with [pred iterable]
  (loop [s (seq iterable) acc (list)]
        (if (or (empty? s) (not (pred (first s))))
            #(acc s)
            (recur (rest s) (-conj! acc (first s))))))

(defn -partition [n iterable] (-partition-in-steps n n iterable))
(defn -partition-all [n iterable] (-partition-all-in-steps n n iterable))

(defn -partition-in-steps [n step iterable]
  (--map (list (-take n it)) (-take-nth step (-sized-window n iterable))))

(defn -partition-all-in-steps [n step iterable]
  (--map (list (-take n it)) (-take-nth step (-unsized-window iterable))))

(defn -partition-by [f iterable]
  (loop [s (seq (-annotate f iterable))]
        (unless (empty? s)
          (let [g (get (first s) 0)
                #(acc ns) (--split-with (= (get it 0) g) s)]
            (yield (list (--map (get it 1) acc)))
            (recur ns)))))

(defmacro --take-while [form iterable] `(-take-while (fn [it] ~form) ~iterable))
(defmacro --drop-while [form iterable] `(-drop-while (fn [it] ~form) ~iterable))
(defmacro --split-with [form iterable] `(-split-with (fn [it] ~form) ~iterable))
(defmacro --partition-by [form iterable] `(-partition-by (fn [it] ~form) ~iterable))


;; iter trans

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


;; iter stat

(defn -count [iterable]
  (if (sized? iterable)
      (len iterable)
      (--reduce-from (inc acc) 0 iterable)))

(defn -count-by [pred iterable] (-count (-filter pred iterable)))

(defn -frequencies [iterable]
  (let [acc (defaultdict (constantly 0))]
    (--each iterable (+= (get acc it) 1))
    acc))

(defn -group-by [f iterable]
  (let [acc (defaultdict list)]
    (--each iterable (-update! acc (f it) -conj! it))
    acc))

(defmacro --count-by [form iterable] `(-count-by (fn [it] ~form) ~iterable))
(defmacro --group-by [form iterable] `(-group-by (fn [it] ~form) ~iterable))


;; functools

(defn -args [#* args] args)

(defn -kwargs [#** kwargs] kwargs)

(defn -apply [f args] (f #* args))

(defn -funcall [f #* args #** kwargs] (f #* args #** kwargs))

(defn -trampoline [f #* args #** kwargs]
  (loop [r (f #* args #** kwargs)] (if (fn? r) (recur (r)) r)))

(defn -constantly [o] (fn [#* args #** kwargs] o))

(defn -complement [pred] (fn [#* args #** kwargs] (not (pred #* args #** kwargs))))

(defn -partial [f #* largs #** lkwargs]
  (fn [#* rargs #** rkwargs] (f #* largs #* rargs #** lkwargs #** rkwargs)))
(defn -rpartial [f #* rargs #** rkwargs]
  (fn [#* largs #** lkwargs] (f #* largs #* rargs #** lkwargs #** rkwargs)))

(defn -comp-in [fns]
  (if fns
      (--reduce (fn [#* args #** kwargs] (acc (it #* args #** kwargs))) fns)
      identity))
(defn -comp [#* fns] (-comp-in fns))

(defn -juxt-in [fns]
  (fn [#* args #** kwargs] (list (--map (it #* args #** kwargs) fns))))
(defn -juxt [#* fns] (-juxt-in fns))


;; dict get/set/del

;; like getattr/setattr/delattr
(defn -getitem [o k] (get o k))
(defn -setitem [o k v] (setv (get o k) v))
(defn -delitem [o k] (del (get o k)))
(defn -updateitem [o k f #* args #** kwargs]
  (-setitem o k (f (-getitem o k) #* args #** kwargs)))

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


;; dict iter

(defn -contains? [o k]
  (cond (map? o) (in k o)
        (sequence? o) (in k (range (len o)))
        True (raise TypeError)))

(defn -get [o k [d None]]
  (cond (map? o) (.get o k d)
        (sequence? o) (if (in k (range (len o))) (-getitem o k) d)
        True (raise TypeError)))

(defn -get-in [o ks [d None]]
  (loop [s (seq ks) acc o]
        (cond (none? acc) d
              (empty? s) acc
              True (recur (rest s) (-get acc (first s))))))

(defn -items [o]
  (cond (map? o) (.items o)
        (sequence? o) (enumerate o)
        True (raise TypeError)))

(defn -keys [o]
  (cond (map? o) (.keys o)
        (sequence? o) (range (len o))
        True (raise TypeError)))

(defn -vals [o]
  (cond (map? o) (.values o)
        (sequence? o) o
        True (raise TypeError)))

(defn -select-keys [o ks]
  (dict (--map #(it (-get o it)) ks)))

(defn -reduce-kv [f init o]
  (--reduce-from (let [#(k v) it] (f acc k v)) init (-items o)))

(defn -merge-in! [o os]
  (--reduce-from
    (let [#(k v) it] (-assoc! o k v))
    o (-concat-in (-map -items os))))

(defn -merge-with-in! [f o os]
  (--reduce-from
    (let [#(k v) it] (if (-contains? o k) (-update! o k f v) (-assoc! o k v)))
    o (-concat-in (-map -items os))))

(defn -merge! [o #* os] (-merge-in! o os))
(defn -merge-with! [f o #* os] (-merge-with-in! f o os))

(defn -merge-in [os] (-merge-in! (dict) os))
(defn -merge-with-in [f os] (-merge-with-in! f (dict) os))

(defn -merge [#* os] (-merge-in os))
(defn -merge-with [f #* os] (-merge-with-in f os))

(defn -update-keys [o f]
  (dict (--map (let [#(k v) it] #((f k) v)) (-items o))))
(defn -update-vals [o f]
  (dict (--map (let [#(k v) it] #(k (f v))) (-items o))))
(defn -update-vals! [o f]
  (--each (-keys o) (-update! o it f)) o)

(defmacro --reduce-kv [form init o] `(-reduce-kv (fn [acc k v] ~form) ~init ~o))
(defmacro --merge-with-in! [form o os] `(-merge-with-in! (fn [acc it] ~form) ~o ~os))
(defmacro --merge-with! [form o #* os] `(-merge-with! (fn [acc it] ~form) ~o ~@os))
(defmacro --merge-with-in [form os] `(-merge-with-in (fn [acc it] ~form) ~os))
(defmacro --merge-with [form #* os] `(-merge-with (fn [acc it] ~form) ~@os))
(defmacro --update-keys [o form] `(-update-keys ~o (fn [it] ~form)))
(defmacro --update-vals [o form] `(-update-vals ~o (fn [it] ~form)))
(defmacro --update-vals! [o form] `(-update-vals! ~o (fn [it] ~form)))


;; coll op

(defn -emptyitem [o]
  (.clear o))

(defn -intoitem [o x]
  (cond (set? o) (--map (.add o it) iterable)
        (map? o) (--map (let [#(k v) it] (-setitem o k v)) iterable)
        (sequence? o) (--map (.append o it) iterable)
        True (raise TypeError)))

(defn -conjitem [o x]
  (cond (set? o) (.add o x)
        (map? o) (let [#(k v) x] (-setitem o k v))
        (sequence? o) (.append o x)
        True (raise TypeError)))

(defn -disjitem [o x]
  (cond (set? o) (.discard o x)
        True (raise TypeError)))

(defn -popitem [o]
  (cond (set? o) (.pop o)
        (map? o) (.popitem o)
        (sequence? o) (.pop o)
        True (raise TypeError)))

(defn -empty! [o] (doto o (-emptyitem)))
(defn -into! [o iterable] (doto o (-intoitem iterable)))
(defn -conj! [o x] (doto o (-conjitem x)))
(defn -disj! [o x] (doto o (-disjitem x)))
(defn -pop! [o] (doto o (-popitem)))

(defn -peek [o]
  (cond (sequence? o) (-getitem o -1)
        (seq? o) (first o)
        True (raise TypeError)))

(defn -empty [o]
  (if (seq? o) (seq) (.__class__ o)))

(defn -into [o iterable]
  (if (seq? o)
      (loop [s (seq iterable) acc o]
            (if (empty? s)
                acc
                (recur (rest s) (cons (first s) acc))))
      (-into! (.copy o) iterable)))

(defn -conj [o x]
  (if (seq? o) (cons x o) (-conj! (.copy o) x)))

(defn -disj [o x]
  (-disj! (.copy o) x))

(defn -pop [o]
  (if (seq? o) (rest o) (-pop! (.copy o))))



(export
  :objects [
            ;; reduce
            -each -each-indexed -dotimes
            -reduce-from -reductions-from -reduce -reductions
            ;; map filter
            -map* -map -map-indexed -filter -remove
            -mapcat -mapcat-indexed -keep -keep-indexed -annotate -annotate-indexed
            -some -any? -all?
            ;; iter op
            -concat-in -concat -cons -pair -empty? -first -rest
            ;; iter gen
            -iterate -iterate-n -repeat -repeat-n -repeatedly -repeatedly-n -cycle -cycle-n -range
            ;; iter mux
            -zip-in -zip -zip-fill-in -zip-fill -tee -tee-n
            -interleave-in -interleave -interleave-fill-in -interleave-fill -interpose
            ;; iter part
            -nth -nthrest
            -take -drop -take-while -drop-while -take-nth -drop-nth
            -unsized-window -sized-window
            -last -butlast -take-last -drop-last
            -split-at -split-with
            -partition -partition-all -partition-in-steps -partition-all-in-steps -partition-by
            ;; iter trans
            -replace -distinct -dedupe
            ;; iter stat
            -count -count-by -frequencies -group-by
            ;; functools
            -args -kwargs -apply -funcall -trampoline
            -constantly -complement -partial -rpartial -comp-in -comp -juxt-in -juxt
            ;; dict get/set/del
            -getitem -setitem -delitem -updateitem
            -getitem-in -setitem-in -delitem-in -updateitem-in
            -assoc! -dissoc! -update! -assoc-in! -dissoc-in! -update-in!
            -assoc -dissoc -update -assoc-in -dissoc-in -update-in
            ;; dict iter
            -contains? -get -get-in -items -keys -vals -select-keys -reduce-kv
            -merge-in! -merge-with-in! -merge! -merge-with!
            -merge-in -merge-with-in -merge -merge-with
            -update-keys -update-vals -update-vals!
            ;; coll op
            -emptyitem -intoitem -conjitem -disjitem -popitem
            -empty! -into! -conj! -disj! -pop!
            -peek -empty -into -conj -disj -pop
            ]
  :macros [
           ;; threading macros
           -> ->> as-> doto some-> some->> cond-> cond->>
           ;; let macros
           -if-let --if-let -when-let --when-let
           ;; reduce
           --each --each-indexed --dotimes
           --reduce-from --reductions-from --reduce --reductions
           ;; map filter
           --map --map-indexed --filter --remove
           --mapcat --mapcat-indexed --keep --keep-indexed --annotate --annotate-indexed
           --some --any? --all?
           ;; iter gen
           --iterate --iterate-n --repeatedly --repeatedly-n
           ;; iter part
           --take-while --drop-while --split-with --partition-by
           ;; iter stat
           --count-by --group-by
           ;; dict get/set/del
           --updateitem --updateitem-in --update! --update-in! --update --update-in
           ;; dict iter
           --reduce-kv --merge-with-in! --merge-with! --merge-with-in --merge-with
           --update-keys --update-vals --update-vals!
           ])
