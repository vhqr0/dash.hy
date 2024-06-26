(require
  dash *
  dash.monads.state *
  dash.monads.cont *)

(import
  unittest [TestCase]
  dash *
  dash.monads.state *
  dash.monads.cont *)

(defclass TestMonad [TestCase]
  (defn test-box [self]
    (.assertEqual self (-> (box 1) (.map inc) (.unwrap)) 2)
    (.assertEqual self (-> (alet [a (box 1)
                                  b (box 2)
                                  c (box 3)]
                             (+ a b c))
                           (.unwrap))
                  6)
    (.assertEqual self (-> (mlet [#(a b) (box #(1 3))
                                  c (box (+ b 3))]
                             (box (+ a b c)))
                           (.unwrap))
                  10))

  (defn test-either [self]
    (.assertEqual self (-> (alet [a (right 1)
                                  b (right 2)]
                             (+ a b))
                           (.unwrap))
                  3)
    (with [_ (.assertRaises self Exception)]
      (-> (alet [a (right 1)
                 b (left)]
            (+ a b))
          (.unwrap)))
    (.assertEqual self (-> (mlet [a (right 1)
                                  b (right (+ a 2))]
                             (right (+ a b)))
                           (.unwrap))
                  4)
    (with [_ (.assertRaises self Exception)]
      (-> (mlet [a (left)
                 b (right (+ a 2))]
            (right (+ a b)))
          (.unwrap)))
    (.assertEqual self (.unwrap
                         (alet [a (try! 1)
                                b (try! 2)]
                           (+ a b)))
                  3)
    (with [_ (.assertRaises self RuntimeError)]
      (-> (alet [a (try! 1)
                 b (try! (raise RuntimeError))]
            (+ a b))
          (.unwrap))))

  (defn test-state [self]
    (.assertEqual self (.data
                         (mlet [a (state.wrap "hello")
                                b (state! #((+ a " ") (inc s)))]
                           (state! #((+ b "world") (inc s))))
                         1)
                  #("hello world" 3)))

  (defn test-cont [self]
    (.assertEqual self
                  (.unwrap
                    (with-cc exit1
                      (mlet [a (cont.wrap 1)
                             b (cont! (k (inc a)))
                             c (cont! (k (inc b)))]
                        (cont! (k (+ a b c))))))
                  6)
    (.assertEqual self
                  (.unwrap
                    (with-cc exit1
                      (mlet [a (cont.wrap 1)
                             b (cont! (k (inc a)))
                             c (exit1 b)]
                        (cont! (k (+ a b c))))))
                  2)
    (.assertEqual self
                  (.unwrap
                    (with-cc exit1
                      (mlet [a (cont.wrap 1)
                             b (with-cc exit2
                                 (mlet [c (cont.wrap a)
                                        d (cont! (k (inc c)))]
                                   (cont! (k (+ c d)))))
                             e (cont! (k (inc b)))]
                        (cont! (k (+ a b e))))))
                  8)
    (.assertEqual self
                  (.unwrap
                    (with-cc exit1
                      (mlet [a (cont.wrap 1)
                             b (with-cc exit2
                                 (mlet [c (cont.wrap a)
                                        d (cont! (k (inc c)))]
                                   (exit1 (+ c d))))
                             e (cont! (k (inc b)))]
                        (cont! (k (+ a b e))))))
                  3)
    (.assertEqual self
                  (.unwrap
                    (with-cc exit1
                      (mlet [a (cont.wrap 1)
                             b (with-cc exit2
                                 (mlet [c (cont.wrap a)
                                        d (exit2 (inc c))]
                                   (cont! (k (+ c d)))))
                             e (cont! (k (inc b)))]
                        (cont! (k (+ a b e))))))
                  6)))

(export
  :objects [TestMonad])
