(require
  dash.dash.polyfill *)

(import
  typing [Any Optional]
  dash.dash.polyfill *)

(setv Seq (py "Optional['Cons']")
      ConsPair (py "tuple[Any, Seq]")
      OptionalConsPair (py "Optional[ConsPair]"))



(defn cons? [o] (isinstance o Cons))
(defn seq?  [o] (or (none? o) (cons? o)))

(defn cons [first [rest None]] (PairCons first rest))
(defn seq [[o None]] (cond (cons? o) o (none? o) (NoneCons) True (IterCons o)))
(defmacro --lazy-seq [#* body] `(-lazy-seq (fn [] ~@body)))
(defn -lazy-seq [lazy-func] (LazyCons lazy-func))

(defn pair   [s] (. (seq s) pair))
(defn empty? [s] (none? (pair s)))
(defn first  [s] (let [p (pair s)] (when p (get p 0))))
(defn rest   [s] (let [p (pair s)] (when p (get p 1))))



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
          True (.format "({} . {})" (get self._pair 0) (get self._pair 1))))

  (defn __repr__ [self]
    (str self)))

(defclass SeqIter []
  (defn __init__ [self seq]
    (setv self.seq seq))

  (defn __iter__ [self]
    self)

  (defn __next__ [self]
    (let [p (pair self.seq)]
      (if (none? p)
          (raise StopIteration)
          (do
            (setv self.seq (get p 1))
            (get p 0))))))



(defclass NoneCons [Cons]
  (setv _pair None)
  (defn [property] pair [self] self._pair))

(defclass PairCons [Cons]
  (defn __init__ [self first rest] (setv self._pair #(first (seq rest))))
  (defn [property] pair [self] self._pair))

(defclass LazyCons [Cons]
  (defn __init__ [self lazy-func] (setv self.lazy-func lazy-func))
  (defn [property] pair [self]
    (when (not (hasattr self "_pair")) (setv self._pair (pair (self.lazy-func))))
    self._pair))

(defclass IterCons [LazyCons]
  (defn __init__ [self iterable]
    (defn lazy-func []
      (try (let [it (iter iterable)] (cons (next it) it)) (except [StopIteration] (NoneCons))))
    (. (super) (__init__ lazy-func))))



(export
  :objects [cons? seq? cons seq -lazy-seq pair empty? first rest]
  :macros [--lazy-seq])
