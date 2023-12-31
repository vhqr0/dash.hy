(require
  dash.dash.polyfill *)

(import
  dash.dash.polyfill *)


;; cons

(setv cons-slots #("car" "cdr"))

(defn cons-init [self car [cdr None]]
  (setv #(self.car self.cdr) #(car cdr)))

(defn cons-eq [self o]
  (and (cons? o) (= self.car o.car) (= self.cdr o.cdr)))

(defn cons-str [self]
  (loop [acc (list) o self]
        (if (cons? o)
            (recur (do (.append acc (car o)) acc) (cdr o))
            (.format "({}{})"
                     (.join " " (map str acc))
                     (if (none? o) "" (.format " . {}" o))))))

(setv cons (type "cons" #()
                 {"__slots__" cons-slots
                  "__init__"  cons-init
                  "__eq__"    cons-eq
                  "__str__"   cons-str
                  "__repr__"  cons-str}))

(defn cons? [o] (isinstance o cons))
(defn car   [o] o.car)
(defn cdr   [o] o.cdr)
(defn caar  [o] (car (car o)))
(defn cadr  [o] (car (cdr o)))
(defn cdar  [o] (cdr (car o)))
(defn cddr  [o] (cdr (cdr o)))

(defn car-safe [o]
  (cond (cons? o) (car o)
        (none? o) None
        True (raise TypeError)))

(defn cdr-safe [o]
  (cond (cons? o) (cdr o)
        (none? o) None
        True (raise TypeError)))

(defn setcar [o v] (setv o.car v) o)
(defn setcdr [o v] (setv o.cdr v) o)


;; conlist

(defn conlist [#* iterable]
  (conlist-in iterable))

(defn conlist-in [iterable]
  (let [acc None]
    (for [o iterable] (setv acc (cons o acc)))
    (conlist-reverse acc)))

(defn conlist-iter [self]
  (loop [o self]
        (unless (none? o)
          (if (cons? o)
              (do (yield (car o)) (recur (cdr o)))
              (raise TypeError)))))

(defn conlist-reverse [o]
  (loop [acc None o o]
        (cond (none? o) acc
              (cons? o) (recur (cons (car o) acc) (cdr o))
              True (raise TypeError))))


;; itercons

(defn iter-cons [o iterable]
  (yield o) (yield-from iterable))

(defn iter-decons [iterable]
  (let [it (iter iterable)]
    (try (cons (next it) it) (except [StopIteration]))))


;; delay

(setv delay-slots #("v" "p"))

(defn delay-init [self v [p False]]
  (setv #(self.v self.p) #(v p)))

(defn delay-str [self]
  (.format "<delay {}={}>" (if self.p "realized" "unrealized") self.v))

(setv delay (type "delay" #()
                  {"__slots__" delay-slots
                   "__init__"  delay-init
                   "__str__"   delay-str
                   "__repr__"  delay-str}))

(defn delay? [o] (isinstance o delay))
(defn realized? [o] o.p)

(defn realize [o]
  (unless (realized? o)
    (setv #(o.v o.p) #((o.v) True)))
  o.v)

(defn force [o]
  (if (not (delay? o)) o (realize o)))

(defmacro lazy [#* body]
  `(delay (fn [] ~@body)))


;; seq

(defn seq-trampoline [o]
  (fn []
    (loop [o o]
          (cond (none? o) None
                (cons? o) (cons (car o) (seq (cdr o)))
                (delay? o) (recur (realize o))
                (iterable? o) (recur (iter-decons o))
                True (raise TypeError)))))

(defn seq-init [self [o None] [p False]]
  (if (or (none? o) p)
      (delay.__init__ self o True)
      (delay.__init__ self (seq-trampoline o) False)))

(defn seq-iter [self]
  (loop [o self]
        (let [it (realize o)]
          (unless (none? it)
            (yield (car it))
            (recur (cdr it))))))

(defn seq-bool [self]
  (not (none? (realize self))))

(defn seq-str [self]
  (loop [acc (list) o self]
    (if (realized? o)
        (let [it (realize o)]
          (if it
              (recur (do (.append acc (car it)) acc) (cdr it))
              (.format "({})" (.join " " (map str acc)))))
        (.format "({} . {})" (.join " " (map str acc)) (delay-str o)))))

(setv seq (type "seq" #(delay)
                {"__slots__" #()
                 "__init__"  seq-init
                 "__iter__"  seq-iter
                 "__str__"   seq-str
                 "__repr__"  seq-str}))

(defn seq? [o] (isinstance o seq))
(defn seqable? [o] (or (none? o) (iterable? o)))

(defn seq-cons [o seqable]
  (seq (cons o (seq seqable)) True))

(defn seq-decons [seqable]
  (realize (seq seqable)))

(defmacro lazy-seq [#* body]
  `(seq (lazy ~@body)))


;; seqable

(defn decons [seqable]
  (unless (none? seqable)
    ((if (seq? seqable) seq-decons iter-decons) seqable)))

(defn empty? [seqable] (none? (decons seqable)))
(defn first [seqable] (car-safe (decons seqable)))
(defn rest [seqable] (cdr-safe (decons seqable)))



(export
  :objects [
            ;; cons
            cons cons? car cdr caar cadr cdar cddr car-safe cdr-safe setcar setcdr
            ;; conlist
            conlist conlist-in conlist-iter conlist-reverse
            ;; iter cons
            iter-cons iter-decons
            ;; delay
            delay delay? realized? force
            ;; seq
            seq seq? seq-cons seq-decons
            ;; seqable
            seqable? decons empty? first rest
            ]
  :macros [lazy lazy-seq])
