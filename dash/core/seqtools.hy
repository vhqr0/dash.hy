(require
  dash.core.polyfill *
  dash.core.monad *
  dash.core.sequence *)

(import
  dash.core.polyfill *
  dash.core.monad *
  dash.core.sequence *
  itertools)


;;; transducer tools

(defn halt-when [pred [exitfn identity]]
  (let [halt? False]
    (make-transducer
      :nonlocals [halt?]
      :comp (if halt? acc (rf acc))
      :step (if (pred it)
                (do
                  (setv halt? True)
                  (reduced (exitfn acc)))
                (rf acc it)))))

(defn halt-unless [pred [exitfn identity]]
  (halt-when (complement pred) exitfn))

(defn xzip [iterable]
  (let [#(sentinel i) #((object) (iter iterable))]
    (make-transducer
      :step (let [x (try
                      (next i)
                      (except [StopIteration]
                        sentinel))]
              (if (is x sentinel)
                  (reduced acc)
                  (rf acc #(x it)))))))



(setv concat itertools.chain
      concat-in itertools.chain.from-iterable)

(setv cat
      (make-transducer
        :step (reducex-step rf acc it)))

(defn mapcat [#* args]
  (match args
         #(f) (comp (map f) cat)
         #(f iterable) (->> iterable (map f) concat-in)
         _ (raise IndexError)))

(defmacro ap-mapcat [#* args]
  (match args
         #(form) `(mapcat (fn [it] ~form))
         #(form iterable) `(mapcat (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defn keep [#* args]
  (match args
         #(f) (comp (map f) (remove none?))
         #(f iterable) (->> iterable (map f) (remove none?))
         _ (raise IndexError)))

(defmacro ap-keep [#* args]
  (match args
         #(form) `(keep (fn [it] ~form))
         #(form iterable) `(keep (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defn some [f iterable]
  (first (keep f iterable)))

(defmacro ap-some [form iterable]
  `(some (fn [it] ~form) ~iterable))

(defn any? [#* args]
  (match args
         #(iterable) (any iterable)
         #(pred iterable) (any (map pred iterable))
         _ (raise IndexError)))

(defn all? [#* args]
  (match args
         #(iterable) (all iterable)
         #(pred iterable) (all (map pred iterable))
         _ (raise IndexError)))

(defmacro ap-any? [form iterable]
  `(any? (fn [it] ~form) ~iterable))

(defmacro ap-all? [form iterable]
  `(all? (fn [it] ~form) ~iterable))



(defn nth [iterable n]
  (first (nthrest iterable n)))

(defn nthrest [iterable n]
  (assert (>= n 0))
  (loop [s (seq iterable) n n]
        (cond (empty? s) (raise IndexError)
              (<= n 0) s
              True (recur (rest s) (dec n)))))

(defn take [#* args]
  (match args
         #(n) (do
                (assert (>= n 0))
                (comp
                  (xzip (iterate inc 0))
                  (make-transducer
                    :step (let [#(index it) it]
                            (if (>= index n)
                                (reduced acc)
                                (rf acc it))))))
         #(n iterable) (do
                         (assert (>= n 0))
                         (loop [s (seq iterable) n n]
                               (unless (or (<= n 0) (empty? s))
                                 (yield (first s))
                                 (recur (rest s) (dec n)))))
         _ (raise IndexError)))

(defn drop [#* args]
  (match args
         #(n) (do
                (assert (>= n 0))
                (comp
                  (xzip (iterate inc 0))
                  (make-transducer
                    :step (let [#(index it) it]
                            (if (>= index n)
                                (rf acc it)
                                acc)))))
         #(n iterable) (do
                         (assert (>= n 0))
                         (loop [s (seq iterable) n n]
                               (if (or (<= n 0) (empty? s))
                                   s
                                   (recur (rest s) (dec n)))))))

(defn split-at [n iterable]
  (assert (>= n 0))
  (loop [s (seq iterable) acc (list) n n]
        (if (or (<= n 0) (empty? s))
            #(acc s)
            (recur (rest s) (list-conj! acc (first s)) (dec n)))))

(defn take-while [#* args]
  (match args
         #(pred) (make-transducer
                   :step (if (pred it)
                             (rf acc it)
                             (reduced acc)))
         #(pred iterable) (loop [s (seq iterable)]
                                (unless (or (empty? s) (not (pred (first s))))
                                  (yield (first s))
                                  (recur (rest s))))
         _ (raise IndexError)))

(defn drop-while [#* args]
  (match args
         #(pred) (let [start? False]
                   (make-transducer
                     :nonlocals [start?]
                     :step (cond start? (rf acc it)
                                 (pred it) acc
                                 True (do
                                        (setv start? True)
                                        (rf acc it)))))
         #(pred iterable) (loop [s (seq iterable)]
                                (if (or (empty? s) (not (pred (first s))))
                                    s
                                    (recur (rest s))))
         _ (raise IndexError)))

(defn split-with [pred iterable]
  (loop [s (seq iterable) acc (list)]
        (if (or (empty? s) (not (pred (first s))))
            #(acc s)
            (recur (rest s) (list-conj! acc (first s))))))

(defmacro ap-take-while [#* args]
  (match args
         #(form) `(take-while (fn [it] ~form))
         #(form iterable) `(take-while (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defmacro ap-drop-while [#* args]
  (match args
         #(form) `(drop-while (fn [it] ~form))
         #(form iterable) `(drop-while (fn [it] ~form) ~iterable)
         _ (raise IndexError)))

(defmacro ap-split-with [form iterable]
  `(split-with (fn [it] ~form) ~iterable))

(defn take-nth [#* args]
  (match args
         #(n) (do
                (assert (>= n 1))
                (comp
                  (xzip (iterate inc 0))
                  (make-transducer
                    :step (let [#(index it) it]
                            (if (zero? (% index n))
                                (rf acc it)
                                acc)))))
         #(n iterable) (do
                         (assert (>= n 1))
                         (loop [s (seq iterable) i 0]
                               (unless (empty? s)
                                 (when (zero? (% i n))
                                   (yield (first s)))
                                 (recur (rest s) (inc i)))))
         _ (raise IndexError)))



(defn window [iterable]
  (loop [s (seq iterable)]
        (unless (empty? s)
          (yield s)
          (recur (rest s)))))

(defn sized-window [n iterable]
  (assert (>= n 1))
  (loop [head (seq iterable) tail (drop (dec n) head)]
        (unless (empty? tail)
          (yield head)
          (recur (rest head) (rest tail)))))

(defn sized-window-all [n iterable]
  (assert (>= n 0))
  (loop [head (seq iterable) tail (drop n head)]
        (do
          (yield head)
          (unless (empty? tail)
            (recur (rest head) (rest tail))))))

(defn last [iterable]
  (loop [s (seq iterable)]
        (cond (empty? s) None
              (empty? (rest s)) (first s)
              True (recur (rest s)))))

(defn butlast [iterable]
  (loop [s (seq iterable)]
        (when (and (not (empty? s)) (not (empty? (rest s))))
          (yield (first s))
          (recur (rest s)))))

(defn take-last [n iterable]
  (assert (>= n 0))
  (last (sized-window-all n iterable)))

(defn drop-last [n iterable]
  (assert (>= n 0))
  (map first (sized-window (inc n) iterable)))



(defn partition [#* args]
  (match args
         #(n) (do
                (assert (>= n 1))
                (let [part (list)]
                  (make-transducer
                    :nonlocals [part]
                    :step (let [it (list-conj! part it)]
                            (if (>= (len it) n)
                                (do
                                  (setv part (list))
                                  (rf acc it))
                                acc)))))
         #(n iterable) (partition n n iterable)
         #(n step iterable) (do
                              (assert (and (>= n 1) (>= step 1)))
                              (ap-map
                                (list (take n it))
                                (take-nth step (sized-window n iterable))))
         _ (raise IndexError)))

(defn partition-all [#* args]
  (match args
         #(n) (do
                (assert (>= n 1))
                (let [part (list)]
                  (make-transducer
                    :nonlocals [part]
                    :comp (rf (if part
                                  (unreduced (rf acc part))
                                  acc))
                    :step (let [it (list-conj! part it)]
                            (if (>= (len it) n)
                                (do
                                  (setv part (list))
                                  (rf acc it))
                                acc)))))
         #(n iterable) (partition-all n n iterable)
         #(n step iterable) (do
                              (assert (and (>= n 1) (>= step 1)))
                              (ap-map
                                (list (take n it))
                                (take-nth step (window iterable))))
         _ (raise IndexError)))

(defn partition-by [#* args]
  (match args
         #(f) (let [part (list) g (object)]
                (make-transducer
                  :nonlocals [part g]
                  :comp (rf (if part (unreduced (rf acc part)) acc))
                  :step (let [ng (f it)]
                          (if (= ng g)
                              (do
                                (.append part it)
                                acc)
                              (let [op part]
                                (setv #(part g) #([it] ng))
                                (if part (rf acc op) acc))))))
         #(f iterable) (loop [s (seq iterable) acc (list) g (object)]
                             (if (empty? s)
                                 (when acc
                                   (yield acc))
                                 (let [ng (f (first s))]
                                   (when (and (!= ng g) acc)
                                     (yield acc)
                                     (setv acc (list)))
                                   (recur (rest s) (list-conj! acc (first s)) ng))))
         _ (raise IndexError)))

(defmacro ap-partition-by [form iterable]
  `(partition-by (fn [it] ~form) ~iterable))



(defn iterate [#* args]
  (match args
         #(f init) (loop [acc init]
                         (do
                           (yield acc)
                           (recur (f acc))))
         #(n f init) (take n (iterate f init))
         _ (raise IndexError)))

(defmacro ap-iterate [#* args]
  (match args
         #(form init) `(iterate (fn [it] ~form) ~init)
         #(n form init) `(iterate ~n (fn [it] ~form) ~init)
         _ (raise IndexError)))

(defn repeat [#* args]
  (match args
         #(x) (loop [] (do (yield x) (recur)))
         #(n x) (take n (repeat x))
         _ (raise IndexError)))

(defn repeatedly [#* args]
  (match args
         #(f) (loop [] (do (yield (f)) (recur)))
         #(n f) (take n (repeatedly f))
         _ (raise IndexError)))

(defmacro ap-repeatedly [#* args]
  (match args
         #(form) `(repeatedly (fn [] ~form))
         #(n form) `(repeatedly ~n (fn [] ~form))
         _ (raise IndexError)))

(defn cycle [#* args]
  (match args
         #(iterable) (concat-in (repeat (seq iterable)))
         #(n iterable) (concat-in (repeat n (seq iterable)))
         _ (raise IndexError)))

(defn interleave [#* iterables]
  (concat-in (zip #* iterables)))

(defn interpose [#* args]
  (match args
         #(sep) (comp
                  (make-transducer
                    :step (reducex-step rf acc #(sep it)))
                  (drop 1))
         #(sep iterable) (->> iterable (interleave (repeat sep)) (drop 1))
         _ (raise IndexError)))

(defn group-by [f iterable]
  (import
    dash.core.colltools [conj! update!]
    collections [defaultdict])
  (ap-reduce
    (update! acc (f it) conj! it)
    (defaultdict list) iterable))

(defmacro ap-group-by [form iterable]
  `(group-by (fn [it] ~form) ~iterable))

(defn replace [#* args]
  (import dash.core.colltools [-get])
  (defn replace-fn [smap]
    (fn [it] (-get smap it it)))
  (match args
         #(smap) (map (replace-fn smap))
         #(smap iterable) (->> iterable (map (replace-fn smap)))
         _ (raise IndexError)))

(defn distinct [#* args]
  (import dash.core.colltools [conj!])
  (defn distinct-pred []
    (let [seen (set)]
      (fn [it]
        (let [first-seen (not-in it seen)]
          (when first-seen
            (conj! seen it))
          first-seen))))
  (match args
         #() (filter (distinct-pred))
         #(iterable) (->> iterable (filter (distinct-pred)))
         _ (raise IndexError)))

(defn dedupe [#* args]
  (defn dedupe-pred []
    (let [last-seen (object)]
      (fn [it]
        (nonlocal last-seen)
        (let [first-seen (!= it last-seen)]
          (when first-seen
            (setv last-seen it))
          first-seen))))
  (match args
         #() (filter (dedupe-pred))
         #(iterable) (->> iterable (filter (dedupe-pred)))
         _ (raise IndexError)))

(defn flatten [iterable]
  (ap-for iterable
          (if (coll? it)
              (yield-from (flatten it))
              (yield it))))



(export
  :objects [halt-when halt-unless xzip
            concat concat-in cat mapcat keep some any? all?
            nth nthrest take drop split-at take-while drop-while split-with take-nth
            window sized-window sized-window-all last butlast take-last drop-last
            partition partition-all partition-by
            iterate repeat repeatedly cycle interleave interpose
            group-by replace distinct dedupe flatten]
  :macros [ap-mapcat ap-keep ap-some ap-any? ap-all?
           ap-take-while ap-drop-while ap-split-with ap-partition-by
           ap-iterate ap-repeatedly ap-group-by])
