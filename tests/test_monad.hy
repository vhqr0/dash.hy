(require
  dash *
  dash.monad *)

(import
  unittest [TestCase]
  dash *
  dash.monad *)

(defclass TestMonad [TestCase]
  (defn test-mid [self]
    (.assertEqual self (-> (mid 1) (.map inc) (.unwrap)) 2)
    (.assertEqual self (-> (alet [a (mid 1)
                                  b (mid 2)
                                  c (mid 3)]
                             (+ a b c))
                           (.unwrap))
                  6)
    (.assertEqual self (-> (mlet [#(a b) (mid #(1 3))
                                  c (mid (+ b 3))]
                             (mid (+ a b c)))
                           (.unwrap))
                  10))

  (defn test-maybe [self]
    (.assertEqual self (-> (alet [a (just 1)
                                  b (just 2)]
                             (+ a b))
                           (.unwrap))
                  3)
    (.assertIsNone self (-> (alet [a (just 1)
                                   b (nothing)]
                              (+ a b))
                            (.unwrap)))
    (.assertEqual self (-> (mlet [a (just 1)
                                  b (just (+ a 2))]
                             (just (+ a b)))
                           (.unwrap))
                  4)
    (.assertIsNone self (-> (mlet [a (nothing)
                                   b (just (+ a 2))]
                              (just (+ a b)))
                            (.unwrap))))

  (defn test-try [self]
    (.assertEqual self (.unwrap
                         (alet [a (mtry! 1)
                                b (mtry! 2)]
                           (+ a b)))
                  3)
    (with [_ (.assertRaises self RuntimeError)]
      (-> (alet [a (mtry! 1)
                 b (mtry! (raise RuntimeError))]
            (+ a b))
          (.unwrap))))

  (defn test-state [self]
    (.assertEqual self ((mlet [a (state.wrap "hello")
                               b (state! #((+ a " ") (inc s)))]
                          (state! #((+ b "world") (inc s))))
                         1)
                  #("hello world" 3)))

  (defn test-cont [self]
    (.assertEqual self
                  ((callCC! exit1
                            (mlet [a (cont.wrap 1)
                                   b (cont! (k (inc a)))
                                   c (cont! (k (inc b)))]
                              (cont! (k (+ a b c)))))
                    identity)
                  6)
    (.assertEqual self
                  ((callCC! exit1
                            (mlet [a (cont.wrap 1)
                                   b (cont! (k (inc a)))
                                   c (exit1 b)]
                              (cont! (k (+ a b c)))))
                    identity)
                  2)
    (.assertEqual self
                  ((callCC! exit1
                            (mlet [a (cont.wrap 1)
                                   b (callCC! exit2
                                              (mlet [c (cont.wrap a)
                                                     d (cont! (k (inc c)))]
                                                (cont! (k (+ c d)))))
                                   e (cont! (k (inc b)))]
                              (cont! (k (+ a b e)))))
                    identity)
                  8)
    (.assertEqual self
                  ((callCC! exit1
                            (mlet [a (cont.wrap 1)
                                   b (callCC! exit2
                                              (mlet [c (cont.wrap a)
                                                     d (cont! (k (inc c)))]
                                                (exit1 (+ c d))))
                                   e (cont! (k (inc b)))]
                              (cont! (k (+ a b e)))))
                    identity)
                  3)
    (.assertEqual self
                  ((callCC! exit1
                            (mlet [a (cont.wrap 1)
                                   b (callCC! exit2
                                              (mlet [c (cont.wrap a)
                                                     d (exit2 (inc c))]
                                                (cont! (k (+ c d)))))
                                   e (cont! (k (inc b)))]
                              (cont! (k (+ a b e)))))
                    identity)
                  6)))

(export
  :objects [TestMonad])
