(require
  dash.core *)

(import
  dash.core *
  dash.metaclasses [cast-meta]
  dataclasses [dataclass]
  typing [Any])



(defn completing [f [cf identity]]
  (fn [#* args]
    (match args
           #() (f)
           #(acc) (cf acc)
           #(acc it) (f acc it)
           _ (raise IndexError))))

;; use cast-meta to avoid creating nested reduced data
(defclass [(dataclass :order True :slots True)] reduced [:metaclass cast-meta]
  #^ Any data)

(defn reduced? [x] (isinstance x reduced))
(defn unreduced [x] (match x (reduced v) v _ x))

(defn t-reduce [#* args]
  (match args
         #(f iterable) (t-reduce f (f) iterable)
         #(f init iterable) (loop [s (seq iterable) acc init]
                                  (if (or (empty? s) (reduced? acc))
                                      (f (unreduced acc))
                                      (recur (rest s) (f acc (first s)))))
         _ (raise IndexError)))

(defn transduce [#* args]
  (match args
         #(xform f iterable) (transduce xform f (f) iterable)
         #(xform f init iterable) (t-reduce (xform f) init iterable)
         _ (raise IndexError)))

(defn t-into [c xform iterable]
  (transduce xform (completing conj) c iterable))

(defn t-into! [c xform iterable]
  (transduce xform (completing conj!) c iterable))

(defn eduction [xform iterable]
  (let [xconj! (xform (completing conj!))]
    (loop [s (seq iterable) acc (list)]
          (if acc
              (match acc
                     (reduced acc) (yield-from (xconj! acc))
                     _ (do
                         (yield-from acc)
                         (recur s (list))))
              (if (empty? s)
                  (yield-from (xconj! acc))
                  (recur (rest s) (xconj! acc (first s))))))))



(defmacro make-transducer [#* body]
  (let [meta (dict (partition-all 2 body))]
    `(fn [rf]
       (fn [#* args]
         ~@(ap-when (-get meta ':nonlocals)
                    `((nonlocal ~@it)))
         (match args
                #() ~(-get meta ':init '(rf))
                #(acc) ~(-get meta ':comp '(rf acc))
                #(acc it) ~(-get meta ':step '(rf acc it))
                _ (raise IndexError))))))

(setv t-cat
      (make-transducer
        :step (loop [s (seq it) acc acc]
                    (if (or (empty? s) (reduced? acc))
                        acc
                        (recur (rest s) (rf acc (first s)))))))

(defn t-halt-when [pred]
  (let [halt? False]
    (make-transducer
      :nonlocals [halt?]
      :comp (if halt? acc (rf acc))
      :step (if (pred it)
                (do
                  (setv halt? True)
                  (reduced acc))
                (rf acc it)))))

(defn t-map [f]
  (make-transducer
    :step (rf acc (f it))))

(defn t-filter [pred]
  (make-transducer
    :step (if (pred it)
              (rf acc it)
              acc)))

(defn t-remove [pred]
  (make-transducer
    :step (if (pred it)
              acc
              (rf acc it))))

(defn t-mapcat [f]
  (comp (t-map f) t-cat))

(defn t-keep [f]
  (comp (t-map f) (t-remove none?)))

(defn t-take [n]
  (assert (>= n 0))
  (make-transducer
    :nonlocals [n]
    :step (if (pos? n)
              (do
                (-= n 1)
                (if (pos? n)
                    (rf acc it)
                    (reduced (rf acc it))))
              acc)))

(defn t-drop [n]
  (assert (>= n 0))
  (make-transducer
    :nonlocals [n]
    :step (if (pos? n)
              (do
                (-= n 1)
                acc)
              (rf acc it))))

(defn t-take-while [pred]
  (make-transducer
    :step (if (pred it)
              (rf acc it)
              (reduced acc))))

(defn t-drop-while [pred]
  (let [start? False]
    (make-transducer
      :nonlocals [start?]
      :step (if start?
                (rf acc it)
                (if (pred it)
                    acc
                    (do
                      (setv start? True)
                      (rf acc it)))))))

(defn t-take-nth [n]
  (assert (>= n 1))
  (let [i n]
    (make-transducer
      :nonlocals [i]
      :step (if (>= i n)
                (do
                  (setv i 1)
                  (rf acc it))
                (do
                  (+= i 1)
                  acc)))))

(defn t-drop-nth [n]
  (assert (>= n 1))
  (let [i n]
    (make-transducer
      :nonlocals [i]
      :step (if (>= i n)
                (do
                  (setv i 1)
                  acc)
                (do
                  (+= i 1)
                  (rf acc it))))))

(defn t-partition [n]
  (assert (>= n 1))
  (let [p (list)]
    (make-transducer
      :nonlocals [p]
      :step (do
              (.append p it)
              (if (>= (len p) n)
                  (let [op p]
                    (setv p (list))
                    (rf acc op))
                  acc)))))

(defn t-partition-all [n]
  (assert (>= n 1))
  (let [p (list)]
    (make-transducer
      :nonlocals [p]
      :comp (rf (if p (unreduced (rf acc p)) acc))
      :step (do
              (.append p it)
              (if (>= (len p) n)
                  (let [op p]
                    (setv p (list))
                    (rf acc op))
                  acc)))))

(defn t-partition-by [f]
  (let [p (list) g (object)]
    (make-transducer
      :nonlocals [p g]
      :comp (rf (if p (unreduced (rf acc p)) acc))
      :step (let [ng (f it)]
              (if (= ng g)
                  (do
                    (.append p it)
                    acc)
                  (let [op p]
                    (setv #(p g) #([it] ng))
                    (if p (rf acc op) acc)))))))



(export
  :objects [completing reduced reduced? unreduced
            t-reduce transduce t-into t-into! eduction
            t-cat t-halt-when t-map t-filter t-remove t-mapcat t-keep
            t-take t-drop t-take-while t-drop-while t-take-nth t-drop-nth
            t-partition t-partition-all t-partition-by]
  :macros [make-transducer])
