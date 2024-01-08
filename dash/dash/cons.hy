(require
  dash.dash.polyfill *)

(import
  dash.dash.polyfill *
  dash.strtools :as s)


;; metas

;; for class C, object o: C(o) cast object o to C, if o is instance of
;; C, avoid creating new object, return o itself.
(defn cast-meta-call [self [o None]]
  (if (isinstance o self) o (.__call__ (super cast-meta self) o)))
(setv cast-meta (type "cast-meta" #(type) {"__call__" cast-meta-call}))


;; cons

(setv cons-slots #("car" "cdr"))

(defn cons-init [self car [cdr None]]
  (setv #(self.car self.cdr) #(car cdr)))

(defn cons-eq [self o]
  (and (cons? o) (= self.car o.car) (= self.cdr o.cdr)))

(defn cons-str [self]
  (loop [acc (list) o self]
        (cond (cons? o) (recur (do (.append acc (car o)) acc) (cdr o))
              (none? o) (s.format "({})" (s.join-in " " (map str acc)))
              True (s.format "({} . {})" (s.join-in " " (map str acc)) o))))

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

(defn conlist [#* iterable [last None]]
  (conlist-in iterable :last last))

(defn conlist-in [iterable [last None]]
  (conlist-reverse (conlist-in-reverse iterable) :last last))

(defn conlist-in-reverse [iterable [last None]]
  (let [acc last]
    (for [o iterable] (setv acc (cons o acc)))
    acc))

(defn conlist-reverse [o [last None]]
  (loop [acc last o o]
        (cond (none? o) acc
              (cons? o) (recur (cons (car o) acc) (cdr o))
              True (raise TypeError))))

(defn conlist-iter [self]
  (loop [o self]
        (unless (none? o)
          (if (cons? o)
              (do (yield (car o)) (recur (cdr o)))
              (raise TypeError)))))


;; delay

(setv delay-slots #("v" "p"))

(defn delay-init [self v [p False]]
  (setv #(self.v self.p) #(v p)))

(defn delay-str [self]
  (s.format "<delay {}={}>" (if self.p "realized" "unrealized") self.v))

(setv delay (type "delay" #()
                  {"__slots__" delay-slots
                   "__init__"  delay-init
                   "__str__"   delay-str
                   "__repr__"  delay-str}))

(defn delay? [o] (isinstance o delay))
(defn realized? [o] o.p)
(defn realize [o] (unless (realized? o) (setv #(o.v o.p) #((o.v) True))) o.v)
(defn force [o] (if (delay? o) (realize o) o))

(defmacro lazy [#* body]
  `(delay (fn [] ~@body)))


;; seq

(defn seq-trampoline [o]
  (loop [o o]
        (cond (none? o) None
              (cons? o) (cons (car o) (seq (cdr o)))
              (delay? o) (recur (realize o))
              (iter? o) (recur (try (cons (next o) o) (except [StopIteration])))
              (iterable? o) (recur (iter o))
              True (raise TypeError))))

(defn seq-init [self [o None]]
  (cond (none? o) (delay.__init__ self None True)
        (cons? o) (delay.__init__ self (cons (car o) (seq (cdr o))) True)
        True (delay.__init__ self (fn [] (seq-trampoline o)) False)))

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
        (cond (not (realized? o)) (s.format "({} . {})" (s.join-in " " (map str acc)) (delay-str o))
              (none? o.v) (s.format "({})" (s.join-in " " (map str acc)))
              True (recur (do (.append acc (car o.v)) acc) (cdr o.v)))))

(setv seq (cast-meta "seq" #(delay)
                     {"__slots__" #()
                      "__init__"  seq-init
                      "__iter__"  seq-iter
                      "__bool__"  seq-bool
                      "__str__"   seq-str
                      "__repr__"  seq-str}))

(defn seq? [o] (isinstance o seq))
(defn empty? [o] (none? (realize (seq o))))
(defn first [o] (car-safe (realize (seq o))))
(defn rest [o] (cdr-safe (realize (seq o))))

(defmacro lazy-seq [#* body]
  `(seq (lazy ~@body)))



(export
  :objects [
            ;; cons
            cons cons? car cdr caar cadr cdar cddr car-safe cdr-safe setcar setcdr
            ;; conlist
            conlist conlist-in conlist-in-reverse conlist-reverse conlist-iter
            ;; delay
            delay delay? realized? realize force
            ;; seq
            seq seq? empty? first rest
            ]
  :macros [lazy lazy-seq])
