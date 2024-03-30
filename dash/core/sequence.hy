(require
  dash.core.polyfill *
  dash.core.monad *)

(import
  dash.core.polyfill *
  dash.core.monad *
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

     ::clist
     "Objects of cons are not iterable by default."
     "We provide some functions to simulate cons list in other Lisp."
     [clist clist-in clist-into clist-conj clist-reverse clist-iter]

     :::clist.example
     (clist 1 2 3)                         ;; (1 2 3)
     (clist-in (range 3))                  ;; (0 1 2)
     (clist-into (clist 1 2 3) (range 3))  ;; (2 1 0 1 2 3)
     (clist-reverse (clist 1 2 3))         ;; (3 2 1)
     (clist-iter (clist 1 2 3))            ;; 1 2 3
     ]]

  #^ Any car
  (setv #^ Any cdr None)

  (defn __repr__ [self]
    (import dash.strtools :as s)
    (let [#(l t) (loop [acc (list) x self]
                       (if (cons? x)
                           (recur (list-conj! acc (car x)) (cdr x))
                           #(acc x)))]
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


;;; clist

(defn clist [#* iterable]
  (clist-in iterable))

(defn clist-in [iterable]
  (clist-reverse (clist-into None iterable)))

(defn clist-into [l iterable]
  (reduce clist-conj l iterable))

(defn clist-conj [l x]
  (cons x l))

(defn clist-iter [l]
  (loop [l l]
        (unless (none? l)
          (match l
                 (cons x l) (do
                              (yield x)
                              (recur l))
                 _ (raise TypeError)))))

(defn clist-reverse [l]
  (clist-into None (clist-iter l)))


;;; seq

(defmacro seq! [#* body]
  `(seq (delay! ~@body)))

(defclass seq [delay :metaclass cast-meta]
  #[["Clojure style lazy sequence."

     :seqable (Optional (| (cons Any seqable) iterable))
     :seq (delay (Optional (cons Any seq)))

     ::example
     (seq)                             ;; ()
     (seq (clist 1 2 3))               ;; (1 2 3)
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
                (delay? x) (recur (realize! x))
                (iter? x) (recur (try (cons (next x) x) (except [StopIteration])))
                (iterable? x) (recur (iter x))
                True (raise TypeError))))

  (defn __init__ [self [x None]]
    (cond (none? x) (.__init__ (super) None True)
          (cons? x) (.__init__ (super) (cons (car x) (seq (cdr x))) True)
          True      (.__init__ (super) (fn [] (seq.trampoline x)))))

  (defn __iter__ [self]
    (loop [x self]
          (let [it (realize! x)]
            (unless (none? it)
              (yield (car it))
              (recur (cdr it))))))

  (defn __bool__ [self]
    (not (none? (realize! self))))

  (defn __repr__ [self]
    (import dash.strtools :as s)
    (let [#(realized unrealized)
          (loop [acc (list) x self]
                (cond (not (realized? x)) #(acc x)
                      (none? x.data) #(acc None)
                      True (recur (list-conj! acc (car x.data)) (cdr x.data))))]
      (s.format "({}{})"
                (s.join-in " " (map repr realized))
                (if (none? unrealized)
                    ""
                    (s.format " . {}" (delay.__repr__ unrealized)))))))

(defn seq? [x] (isinstance x seq))
(defn empty? [x] (none? (realize! (seq x))))
(defn first [x] (car-safe (realize! (seq x))))
(defn rest [x] (cdr-safe (realize! (seq x))))
(defn second [x] (first (rest x)))


;;; transducer

;; use cast-meta to avoid creating nested reduced data
(defclass [(dataclass :order True :slots True)] reduced [:metaclass cast-meta]
  #^ Any data)

(defn reduced? [x] (isinstance x reduced))
(defn unreduced [x] (match x (reduced v) v _ x))

(defn reducex-step [f init iterable]
  (loop [s (seq iterable) acc init]
        (if (or (empty? s) (reduced? acc))
            acc
            (recur (rest s) (f acc (first s))))))

(defn reducex [#* args]
  (match args
         #(f iterable) (reducex f (f) iterable)
         #(f init iterable) (f (unreduced (reducex-step f init iterable)))
         _ (raise IndexError)))

(defn transduce [#* args]
  (match args
         #(xform f iterable) (transduce xform f (f) iterable)
         #(xform f init iterable) (reducex (xform f) init iterable)
         _ (raise IndexError)))

;; It's obviously that our reducing function doesn't support init.
(defmacro ap-reducex [form init iterable]
  `(reducex (completing (fn [acc it] ~form)) ~init ~iterable))

(defmacro ap-transduce [xform form init iterable]
  `(transduce ~xform (completing (fn [acc it] ~form) ~init ~iterable)))

(defn eduction [xform iterable]
  (let [xconj! (xform (completing list-conj!))]
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



(export
  :objects [
            ;; cons
            cons cons? car cdr caar cadr cdar cddr car-safe cdr-safe setcar setcdr
            ;; clist
            clist clist-in clist-into clist-conj clist-reverse clist-iter
            ;; seq
            seq seq? empty? first rest second
            ;; transducer
            reduced reduced? unreduced reducex-step reducex transduce eduction
            ]
  :macros [seq! ap-reducex ap-transduce])