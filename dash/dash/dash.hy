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
  `(let [~name ~x] ~@(map (fn [it] `(setv ~name ~it)) body) ~name))

(defmacro doto [x #* body]
  (let [$ (hy.gensym)]
    `(let [~$ ~x] ~@(map (fn [it] (-thread-first-form $ it)) body) ~$)))

(defmacro some-> [x #* body]
  (let [$ (hy.gensym)]
    (loop [s (seq body) acc x]
          (if (empty? s)
              acc
              (recur (rest s)
                     `(let [~$ ~acc] (unless (none? ~$) ~(-thread-first-form $ (first s)))))))))

(defmacro some->> [x #* body]
  (let [$ (hy.gensym)]
    (loop [s (seq body) acc x]
          (if (empty? s)
              acc
              (recur (rest s)
                     `(let [~$ ~acc] (unless (none? ~$) ~(-thread-last-form $ (first s)))))))))


;; let macros

(defmacro --if-let [test then else] `(let [it ~test] (if it ~then ~else)))
(defmacro -if-let [binding then else]
  (let [#(name val) binding] `(let [~name ~val] (if ~name ~then ~else))))

(defmacro --when-let [test #* body] `(let [it ~test] (when it ~@body)))
(defmacro -when-let [binding #* body]
  (let [#(name val) binding] `(let [~name ~val] (when ~name ~@body))))


;; side effects

(defmacro --each [iterable #* body] `(let [it None] (for [it ~iterable] ~@body)))
(defn -each [iterable f] (--each iterable (f it)))

(defmacro --each-indexed [iterable #* body]
  `(let [#(it-index it) #(None None)] (for [#(it-index it) (enumerate ~iterable)] ~@body)))
(defn -each-indexed [iterable f] (--each-indexed iterable (f it-index it)))

(defmacro --dotimes [n #* body] `(--each (range ~n) ~@body))
(defn -dotimes [n f] (--dotimes n (f it)))


;; map filter

(defn -map* [f iterable] (--each iterable (yield (f #* it))))

(defmacro --map [form iterable] `(-map (fn [it] ~form) ~iterable))
(defn -map [f iterable] (--each iterable (yield (f it))))

(defmacro --map-indexed [form iterable] `(-map-indexed (fn [it-index it] ~form) ~iterable))
(defn -map-indexed [f iterable] (--each-indexed iterable (yield (f it-index it))))

(defmacro --filter [form iterable] `(-filter (fn [it] ~form) ~iterable))
(defn -filter [pred iterable] (--each iterable (when (pred it) (yield it))))

(defmacro --remove [form iterable] `(-remove (fn [it] ~form) ~iterable))
(defn -remove [pred iterable] (--each iterable (unless (pred it) (yield it))))

(defmacro --mapcat [form iterable] `(-mapcat (fn [it] ~form) ~iterable))
(defn -mapcat [f iterable] (-concat-in (-map f iterable)))

(defmacro --mapcat-indexed [form iterable] `(-mapcat-indexed (fn [it-index it] ~form) ~iterable))
(defn -mapcat-indexed [f iterable] (-concat-in (-map-indexed f iterable)))

(defmacro --keep [form iterable] `(-keep (fn [it] ~form) ~iterable))
(defn -keep [f iterable] (-remove none? (-map f iterable)))

(defmacro --keep-indexed [form iterable] `(-keep-indexed (fn [it-index it] ~form) ~iterable))
(defn -keep-indexed [f iterable] (-remove none? (-map-indexed f iterable)))

(defmacro --annotate [form iterable] `(-annotate (fn [it] ~form) ~iterable))
(defn -annotate [f iterable] (--each iterable (yield #((f it) it))))

(defmacro --annotate-indexed [form iterable] `(-annotate-indexed (fn [it-index it] ~form) ~iterable))
(defn -annotate-indexed [f iterable] (--each-indexed iterable (yield #((f it-index it) it))))


;; reduce

(defmacro --reduce-from [form init iterable] `(let [acc ~init] (--each ~iterable (setv acc ~form)) acc))
(defn -reduce-from [f init iterable] (--reduce-from (f acc it) init iterable))

(defmacro --reductions-from [form init iterable]
  `(let [acc ~init] (yield acc) (--each ~iterable (do (setv acc ~form) (yield acc)))))
(defn -reductions-from [f init iterable] (--reductions-from (f acc it) init iterable))

(defmacro --reduce [form iterable] `(-reduce (fn [acc it] ~form) ~iterable))
(defn -reduce [f iterable] (let [i (iter iterable)] (-reduce-from f (next i) i)))

(defmacro --reductions [form iterable] `(-reductions (fn [acc it] ~form) ~iterable))
(defn -reductions [f iterable] (let [i (iter iterable)] (-reductions-from f (next i) i)))


;; pred

(defmacro --some [form iterable] `(-some (fn [it] ~form) ~iterable))
(defn -some [f iterable] (--each iterable (let [r (f it)] (when r (return r)))) (return None))

(defmacro --any? [form iterable] `(-any? (fn [it] ~form) ~iterable))
(defn -any? [pred iterable] (not (none? (-some pred iterable))))

(defmacro --all? [form iterable] `(-all? (fn [it] ~form it) ~iterable))
(defn -all? [pred iterable] (none? (-some (-complement pred) iterable)))


;; iter op

(defn -concat-in [iterables] (--each iterables (yield-from it)))
(defn -concat [#* iterables] (-concat-in iterables))

(defn -cons [o iterable] (yield o) (yield-from iterable))

(defn -pair   [iterable] (let [it (iter iterable)] (try #((next it) it) (except [StopIteration]))))
(defn -empty? [iterable] (none? (-pair iterable)))
(defn -first  [iterable] (--when-let (-pair iterable) (get it 0)))
(defn -rest   [iterable] (--when-let (-pair iterable) (get it 1)))


;; iter gen

(defmacro --iterate [form init] `(-iterate (fn [it] ~form) ~init))
(defn -iterate [f init] (while True (yield init) (setv init (f init))))
(defmacro --iterate-n [n form init] `(-iterate-n ~n (fn [it] ~form) ~init))
(defn -iterate-n [n f init]
  ;;; avoid additional calculations
  ;; (--dotimes n (yield init) (setv init (f init)))
  (when (>= n 1) (yield init) (--dotimes (dec n) (setv init (f init)) (yield init))))

(defn -repeat [o] (while True (yield o)))
(defn -repeat-n [n o] (--dotimes n (yield o)))

(defmacro --repeatedly [form] `(-repeatedly (fn [] ~form)))
(defn -repeatedly [f] (while True (yield (f))))
(defmacro --repeatedly-n [n form] `(-repeatedly-n ~n (fn [] ~form)))
(defn -repeatedly-n [n f] (--dotimes n (yield (f))))

(defn -cycle [iterable] (-concat-in (-repeat (seq iterable))))
(defn -cycle-n [n iterable] (-concat-in (-repeat-n (seq iterable))))

(defn -range [] (-iterate inc 0))


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
      (--if-let (-drop n iterable) (first it) (raise IndexError))))

(defn -nthrest [iterable n] (--if-let (-drop n iterable) it (raise IndexError)))

(defn -take [n iterable]
  (loop [s (seq iterable) n n]
        (unless (or (<= n 0) (empty? s)) (yield (first s)) (recur (rest s) (dec n)))))

(defn -drop [n iterable]
  (loop [s (seq iterable) n n]
        (if (or (<= n 0) (empty? s)) s (recur (rest s) (dec n)))))

(defmacro --take-while [form iterable] `(-take-while (fn [it] ~form) ~iterable))
(defn -take-while [pred iterable]
  (loop [s (seq iterable)]
        (when (and (not (empty? s)) (pred (first s))) (yield (first s)) (recur (rest s)))))

(defmacro --drop-while [form iterable] `(-drop-while (fn [it] ~form) ~iterable))
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
            (recur (rest s) (doto acc (.append (first s))) (dec n)))))

(defmacro --split-with [form iterable] `(-split-with (fn [it] ~form) ~iterable))
(defn -split-with [pred iterable]
  (loop [s (seq iterable) acc (list)]
        (if (or (empty? s) (not (pred (first s))))
            #(acc s)
            (recur (rest s) (doto acc (.append (first s)))))))

(defn -partition [n iterable] (-partition-in-steps n n iterable))
(defn -partition-in-steps [n step iterable]
  (--map (list (-take n it)) (-take-nth step (-sized-window n iterable))))

(defn -partition-all [n iterable] (-partition-all-in-steps n n iterable))
(defn -partition-all-in-steps [n step iterable]
  (--map (list (-take n it)) (-take-nth step (-unsized-window iterable))))

(defmacro --partition-by [form iterable] `(-partition-by (fn [it] ~form) ~iterable))
(defn -partition-by [f iterable]
  (loop [s (seq (-annotate f iterable))]
        (unless (empty? s)
          (let [g (get (first s) 0)
                #(acc ns) (--split-with (= (get it 0) g) s)]
            (yield (list (--map (get it 1) acc)))
            (recur ns)))))


;; iter stat

(defn -count [iterable] (if (sized? iterable) (len iterable) (--reduce-from (inc acc) 0 iterable)))

(defmacro --count-by [form iterable] `(-count-by (fn [it] ~form) ~iterable))
(defn -count-by [pred iterable] (-count (-filter pred iterable)))

(defn -frequencies [iterable]
  (let [acc (defaultdict (constantly 0))]
    (--each iterable (+= (get acc it) 1))
    acc))

(defmacro --group-by [form iterable] `(-group-by (fn [it] ~form) ~iterable))
(defn -group-by [f iterable]
  (let [acc (defaultdict list)]
    (--each iterable (.append (get acc (f it)) it))
    acc))

(defmacro --reduce-by [key-form reduce-form iterable]
  `(-reduce-by (fn [it] ~key-form) (fn [acc it] ~reduce-form) ~iterable))
(defn -reduce-by [key-fn reduce-fn default-factory iterable]
  (let [acc (defaultdict default-factory)]
    (--each iterable (let [k (key-fn it)] (setv (get acc k) (reduce-fn (get acc k) it))))
    acc))


;; functools

(defn -args [#* args] args)

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
  (if fns (--reduce (fn [#* args #** kwargs] (acc (it #* args #** kwargs))) fns) identity))
(defn -comp [#* fns] (-comp-in fns))

(defn -juxt-in [fns] (fn [#* args #** kwargs] (list (--map (it #* args #** kwargs) fns))))
(defn -juxt [#* fns] (-juxt-in fns))



(export
  :objects [
            ;; side effects
            -each -each-indexed -dotimes
            ;; map filter
            -map* -map -map-indexed -filter -remove
            -mapcat -mapcat-indexed -keep -keep-indexed -annotate -annotate-indexed
            ;; reduce
            -reduce-from -reductions-from -reduce -reductions
            ;; pred
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
            -take -drop -take-while -drop-while -take-nth -drop-nth -unsized-window -sized-window
            -last -butlast -take-last -drop-last -split-at -split-with
            -partition -partition-in-steps -partition-all -partition-all-in-steps -partition-by
            ;; iter stat
            -count -count-by -frequencies -group-by -reduce-by
            ;; functools
            -args -apply -funcall -trampoline -constantly -complement
            -partial -rpartial -comp-in -comp -juxt-in -juxt]
  :macros [
           ;; threading macros
           -> ->> as-> doto some-> some->>
           ;; let macros
           -if-let --if-let -when-let --when-let
           ;; side effects
           --each --each-indexed --dotimes
           ;; map filter
           --map --map-indexed --filter --remove
           --mapcat --mapcat-indexed --keep --keep-indexed --annotate --annotate-indexed
           ;; reduce
           --reduce-from --reductions-from --reduce --reductions
           ;; pred
           --some --any? --all?
           ;; iter gen
           --iterate --iterate-n --repeatedly --repeatedly-n
           ;; iter part
           --take-while --drop-while --split-with --partition-by
           ;; iter stat
           --count-by --group-by --reduce-by])
