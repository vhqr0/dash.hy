(require
  dash.core.polyfill *)

(import
  dash.core.polyfill *
  dash.metaclasses [cast-meta]
  dataclasses [dataclass]
  typing [Any])


;;; cons

(defclass [(dataclass :repr False :order True :slots True)] cons []
  #[["Lisp style cons struct."

     ::example
     (cons 1)                   ;; (1)
     (cons 1 2)                 ;; (1 . 2)
     (cons 1 (cons 2))          ;; (1 2)
     (cons 1 (cons 2 3))        ;; (1 2 . 3)
     (cons 1 (cons 2 (cons 3))) ;; (1 2 3)
     ...

     ::operations
     "Inspired by Emacs Lisp."
     [cons? car cdr caar cadr cdar cddr car-safe cdr-safe setcar setcdr]

     :::operations.example
     (car (cons 1 2))       ;; 1
     (cdr (cons 1 2))       ;; 2
     (car-safe None)        ;; None
     (car-safe (cons 1 2))  ;; 1
     (cdr-safe None)        ;; None
     (cdr-safe (cons 1 2))  ;; 2
     (setcar (cons 1 2) 3)  ;; (3 . 2)
     (setcdr (cons 1 2) 3)  ;; (1 . 3)

     ::conlist
     "Objects of cons are not iterable by default."
     "We provide some functions to simulate cons list in other Lisp."
     [conlist conlist-in conlist-into conlist-conj conlist-reverse conlist-iter]

     :::conlist.example
     (conlist 1 2 3)                           ;; (1 2 3)
     (conlist-in (range 3))                    ;; (0 1 2)
     (conlist-into (conlist 1 2 3) (range 3))  ;; (2 1 0 1 2 3)
     (conlist-reverse (conlist 1 2 3))         ;; (3 2 1)
     (conlist-iter (conlist 1 2 3))            ;; 1 2 3
     ]]

  #^ Any car
  (setv #^ Any cdr None)

  (defn __repr__ [self]
    (import
      dash.core.colltools [conj!]
      dash.strtools :as s)
    (let [#(l t) (loop [acc (list) o self]
                       (if (cons? o)
                           (recur (conj! acc (car o)) (cdr o))
                           #(acc o)))]
      (s.format "({}{})"
                (s.join-in " " (map repr l))
                (if (none? t) "" (s.format " . {}" (repr t)))))))

(defn cons? [x] (isinstance x cons))
(defn car   [x] x.car)
(defn cdr   [x] x.cdr)
(defn caar  [x] (car (car x)))
(defn cadr  [x] (car (cdr x)))
(defn cdar  [x] (cdr (car x)))
(defn cddr  [x] (cdr (cdr x)))

(defn car-safe [x]
  (cond (cons? x) (car x)
        (none? x) None
        True (raise TypeError)))

(defn cdr-safe [x]
  (cond (cons? x) (cdr x)
        (none? x) None
        True (raise TypeError)))

(defn setcar [x v]
  (->> x
       (do (setv x.car v))))

(defn setcdr [x v]
  (->> x
       (do (setv x.cdr v))))


;;; conlist

(defn conlist [#* iterable]
  (conlist-in iterable))

(defn conlist-in [iterable]
  (conlist-reverse (conlist-into None iterable)))

(defn conlist-into [l iterable]
  (reduce conlist-conj l iterable))

(defn conlist-conj [l x]
  (cons x l))

(defn conlist-iter [l]
  (loop [o l]
        (unless (none? o)
          (if (cons? o)
              (do
                (yield (car o))
                (recur (cdr o)))
              (raise TypeError)))))

(defn conlist-reverse [l]
  (conlist-into None (conlist-iter l)))


;;; delay

(defclass [(dataclass :repr False :slots True)] delay []
  #[["Lazy computing wrapper."

     ::example
     (delay (fn [] (+ 1 1)))                                 ;; delay(unrealized=<fn ...>)
     (delay! (+ 1 1))                                        ;; delay(unrealized=<fn ...>)
     (realize (delay! (+ 1 1)))                              ;; 2
     (let [it (delay! (+ 1 1))] (realize it) it)             ;; delay(realized=2)
     (let [it (delay! (+ 1 1))] (realize it) (realized? it)) ;; True

     ::operations
     [delay! delay? realized? realize]
     ]]

  #^ Any data
  (setv #^ bool realized False)

  (defn __repr__ [self]
    (import dash.strtools :as s)
    (s.format "delay({}={})" (if self.realized "realized" "unrealized") (repr self.data))))

(defmacro delay! [#* body]
  `(delay (fn [] ~@body)))

(defn delay? [x] (isinstance x delay))
(defn realized? [x] x.realized)

(defn realize [x]
  (->> x.data
       (do
         (unless (realized? x)
           (setv #(x.data x.realized) #((x.data) True))))))


;;; seq

(defclass seq [delay :metaclass cast-meta]
  #[["Clojure style lazy sequence."

     :seqable (Optional (| (cons Any seqable) iterable))
     :seq (delay (Optional (cons Any seq)))

     ::example
     (seq)                             ;; ()
     (seq (conlist 1 2 3))             ;; (1 2 3)
     (seq (range 3))                   ;; (. (range 3))   => 0 1 2
     (seq (cons 1 (range 3)))          ;; (1 . (range 3)) => 1 0 1 2
     (seq (delay! (cons 1 (range 3)))) ;; (. (delay ...)) => 1 0 1 2
     (seq! (cons 1 (range 3)))         ;; (. (delay ...)) => 1 0 1 2

     ::operations
     [seq! seq? empty? first rest]
     ]]

  (setv __slots__ #())

  (defn [classmethod] trampoline [cls x]
    "Cyclical transform x until find cons or none."
    (loop [x x]
          (cond (none? x) None
                (cons? x) (cons (car x) (cls (cdr x)))
                (delay? x) (recur (realize x))
                (iter? x) (recur (try (cons (next x) x) (except [StopIteration])))
                (iterable? x) (recur (iter x))
                True (raise TypeError))))

  (defn __init__ [self [x None]]
    (cond (none? x) (.__init__ (super) None True)
          (cons? x) (.__init__ (super) (cons (car x) (seq (cdr x))) True)
          True      (.__init__ (super) (fn [] (seq.trampoline x)))))

  (defn __iter__ [self]
    (loop [x self]
          (let [it (realize x)]
            (unless (none? it)
              (yield (car it))
              (recur (cdr it))))))

  (defn __bool__ [self]
    (not (none? (realize self))))

  (defn __repr__ [self]
    (import
      dash.core.colltools [conj!]
      dash.strtools :as s)
    (let [#(realized unrealized)
          (loop [acc (list) x self]
                (cond (not (realized? x)) #(acc x)
                      (none? x.v) #(acc None)
                      True (recur (conj! acc (car x.v)) (cdr x.v))))]
      (s.format "({}{})"
                (s.join-in " " (map repr realized))
                (if (none? unrealized)
                    ""
                    (s.format " . {}" (delay.__repr__ unrealized)))))))

(defmacro seq! [#* body]
  `(seq (delay! ~@body)))

(defn seq? [x] (isinstance x seq))
(defn empty? [x] (none? (realize (seq x))))
(defn first [x] (car-safe (realize (seq x))))
(defn rest [x] (cdr-safe (realize (seq x))))


;;; seqtools

(defn second [x]
  (first (rest x)))

(defn nth [seqable n]
  (first (nthrest seqable n)))

(defn nthrest [seqable n]
  (assert (>= n 0))
  (loop [s (seq seqable) n n]
        (cond (empty? s) (raise IndexError)
              (<= n 0) s
              True (recur (rest s) (dec n)))))

(defn last [seqable]
  (loop [s (seq seqable)]
        (cond (empty? s) None
              (empty? (rest s)) (first s)
              True (recur (rest s)))))

(defn butlast [seqable]
  (loop [s (seq seqable)]
        (when (and (not (empty? s)) (not (empty? (rest s))))
          (yield (first s))
          (recur (rest s)))))

(defn take [n seqable]
  (assert (>= n 0))
  (loop [s (seq seqable) n n]
        (unless (or (<= n 0) (empty? s))
          (yield (first s))
          (recur (rest s) (dec n)))))

(defn drop [n seqable]
  (assert (>= n 0))
  (loop [s (seq seqable) n n]
        (if (or (<= n 0) (empty? s))
            s
            (recur (rest s) (dec n)))))

(defn split-at [n seqable]
  (import dash.core.colltools [conj!])
  (assert (>= n 0))
  (loop [s (seq seqable) acc (list) n n]
        (if (or (<= n 0) (empty? s))
            #(acc s)
            (recur (rest s) (conj! acc (first s)) (dec n)))))

(defn take-while [pred seqable]
  (loop [s (seq seqable)]
        (unless (or (empty? s) (not (pred (first s))))
          (yield (first s))
          (recur (rest s)))))

(defn drop-while [pred seqable]
  (loop [s (seq seqable)]
        (if (or (empty? s) (not (pred (first s))))
            s
            (recur (rest s)))))

(defn split-with [pred seqable]
  (import dash.core.colltools [conj!])
  (loop [s (seq seqable) acc (list)]
        (if (or (empty? s) (not (pred (first s))))
            #(acc s)
            (recur (rest s) (conj! acc (first s))))))

(defmacro ap-take-while [form seqable]
  `(take-while (fn [it] ~form) ~seqable))

(defmacro ap-drop-while [form seqable]
  `(drop-while (fn [it] ~form) ~seqable))

(defmacro ap-split-with [form seqable]
  `(split-with (fn [it] ~form) ~seqable))

(defn window [seqable]
  (loop [s (seq seqable)]
        (unless (empty? s)
          (yield s)
          (recur (rest s)))))

(defn sized-window [n seqable]
  (assert (>= n 1))
  (loop [head (seq seqable) tail (drop (dec n) head)]
        (unless (empty? tail)
          (yield head)
          (recur (rest head) (rest tail)))))

(defn sized-loose-window [n seqable]
  (assert (>= n 0))
  (loop [head (seq seqable) tail (drop n head)]
        (do
          (yield head)
          (unless (empty? tail)
            (recur (rest head) (rest tail))))))

(defn take-last [n seqable]
  (assert (>= n 0))
  (last (sized-loose-window n seqable)))

(defn drop-last [n seqable]
  (assert (>= n 0))
  (map first (sized-window (inc n) seqable)))

(defn take-nth [n seqable]
  (assert (>= n 1))
  (loop [s (seq seqable) i n]
        (unless (empty? s)
          (if (>= i n)
              (do
                (yield (first s))
                (recur (rest s) 1))
              (recur (rest s) (inc i))))))

(defn drop-nth [n seqable]
  (assert (>= n 1))
  (loop [s (seq seqable) i n]
        (unless (empty? s)
          (if (>= i n)
              (recur (rest s) 1)
              (do
                (yield (first s))
                (recur (rest s) (inc i)))))))

(defn partition [#* args]
  (match args
         #(n seqable) (partition n n seqable)
         #(n step seqable) (do
                             (assert (and (>= n 1) (>= step 1)))
                             (ap-map
                               (list (take n it))
                               (take-nth step (sized-window n seqable))))
         _ (raise IndexError)))

(defn partition-all [#* args]
  (match args
         #(n seqable) (partition-all n n seqable)
         #(n step seqable) (do
                             (assert (and (>= n 1) (>= step 1)))
                             (ap-map
                               (list (take n it))
                               (take-nth step (window seqable))))
         _ (raise IndexError)))

(defn partition-by [f seqable]
  (import dash.core.colltools [conj!])
  (loop [s (seq seqable) acc (list) g (object)]
        (if (empty? s)
            (when acc
              (yield acc))
            (let [ng (f (first s))]
              (when (and (!= ng g) acc)
                (yield acc)
                (setv acc (list)))
              (recur (rest s) (conj! acc (first s)) ng)))))

(defmacro ap-partition-by [form seqable]
  `(partition-by (fn [it] ~form) ~seqable))


;;; itertools

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

(defn interpose [sep iterable]
  (drop 1 (interleave (repeat sep) iterable)))

(defn keep [f iterable]
  (->> (map f iterable) (remove none?)))

(defmacro ap-keep [form iterable]
  `(keep (fn [it] ~form) ~iterable))

(defn some [f iterable]
  (first (keep f iterable)))

(defmacro ap-some [form iterable]
  `(some (fn [it] ~form) ~iterable))

(defn group-by [f iterable]
  (import
    dash.core.colltools [conj! update!]
    collections [defaultdict])
  (ap-reduce
    (update! acc (f it) conj! it)
    (defaultdict list) iterable))

(defmacro ap-group-by [form iterable]
  `(group-by (fn [it] ~form) ~iterable))

(defn replace [smap iterable]
  (import dash.core.colltools [-get])
  (ap-map (-get smap it it) iterable))

(defn distinct [iterable]
  (import dash.core.colltools [conj!])
  (let [seen (set)]
    (ap-filter
      (let [first-seen (not-in it seen)]
        (when first-seen
          (conj! seen it))
        first-seen)
      iterable)))

(defn dedupe [iterable]
  (let [last-seen (object)]
    (ap-filter
      (do
        (nonlocal last-seen)
        (let [first-seen (not (= it last-seen))]
          (when first-seen
            (setv last-seen it))
          first-seen))
      iterable)))

(defn flatten [iterable]
  (for [x iterable]
    (if (coll? x) (yield-from (flatten x)) (yield x))))



(export
  :objects [
            ;; cons
            cons cons? car cdr caar cadr cdar cddr car-safe cdr-safe setcar setcdr
            ;; conlist
            conlist conlist-in conlist-into conlist-conj conlist-reverse conlist-iter
            ;; delay
            delay delay? realized? realize
            ;; seq
            seq seq? empty? first rest
            ;; seqtools
            second nth nthrest last butlast take drop split-at take-while drop-while split-with
            window sized-window sized-loose-window take-last drop-last take-nth drop-nth
            partition partition-all partition-by
            ;; itertools
            iterate repeat repeatedly cycle interleave interpose
            keep some group-by replace distinct dedupe flatten
            ]
  :macros [
           ;; delay
           delay!
           ;; seq
           seq!
           ;; seqtools
           ap-take-while ap-drop-while ap-split-with ap-partition-by
           ;; itertools
           ap-iterate ap-repeatedly ap-keep ap-some ap-group-by
           ])
