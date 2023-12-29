(require
  dash.dash.polyfill *)

(import
  typing [Any Optional]
  dash.dash.polyfill *)

(setv Seq (py "Optional['Cons']")
      ConsPair (py "tuple[Any, Seq]")
      OptionalConsPair (py "Optional[ConsPair]"))



(defn cons?    [o] (isinstance o Cons))
(defn seq?     [o] (or (none? o) (cons? o)))
(defn seqable? [o] (or (seq? o) (iterable? o)))

(defn cons [first [rest None]] (PairCons first rest))
(defn seq [[o None]] (cond (cons? o) o (none? o) (NoneCons) True (IterCons o)))
(defmacro --lazy-seq [#* body] `(-lazy-seq (fn [] ~@body)))
(defn -lazy-seq [lazy-func] (LazyCons lazy-func))

(defn seqpair [s] (. (seq s) pair))
(defn empty?  [s] (none? (seqpair s)))
(defn first   [s] (let [pair (seqpair s)] (when pair (get pair 0))))
(defn rest    [s] (let [pair (seqpair s)] (when pair (get pair 1))))



(defclass Cons []
  #^ OptionalConsPair _pair

  (defn [property] #^ OptionalConsPair pair [self]
    (raise NotImplementedError))

  (defn __iter__ [self]
    (SeqIter self))

  (defn __bool__ [self]
    (not (none? self.pair)))

  (defn __str__ [self]
    (cond (not (hasattr self "_pair")) (.format "<{}>" self.__class__.__name__)
          (none? self._pair) "()"
          True (let [#(first rest) self._pair] (.format "({} . {})" first rest))))

  (defn __repr__ [self]
    (str self)))

(defclass SeqIter []
  (defn __init__ [self seq]
    (setv self.seq seq))

  (defn __iter__ [self]
    self)

  (defn __next__ [self]
    (let [pair (seqpair self.seq)]
      (if (none? pair)
          (raise StopIteration)
          (let [#(first rest) pair]
            (setv self.seq rest)
            first)))))



(defclass NoneCons [Cons]
  (setv _pair None)
  (defn [property] pair [self] self._pair))

(defclass PairCons [Cons]
  (defn __init__ [self first rest] (setv self._pair #(first (seq rest))))
  (defn [property] pair [self] self._pair))

(defclass LazyCons [Cons]
  (defn __init__ [self lazy-func] (setv self.lazy-func lazy-func))
  (defn [property] pair [self]
    (when (not (hasattr self "_pair")) (setv self._pair (seqpair (self.lazy-func))))
    self._pair))

(defclass IterCons [LazyCons]
  (defn __init__ [self iterable]
    (defn lazy-func []
      (try (let [it (iter iterable)] (cons (next it) it)) (except [StopIteration] (NoneCons))))
    (. (super) (__init__ lazy-func))))



(export
  :objects [cons? seq? seqable? cons seq -lazy-seq seqpair empty? first rest]
  :macros [--lazy-seq])
