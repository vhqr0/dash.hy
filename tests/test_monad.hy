(require
  dash *
  dash.monad *)

(import
  unittest [TestCase]
  dash *
  dash.monad *
  dash.operator :as o)

(defclass TestMonad [TestCase]
  (defn test-mid [self]
    (.assertEqual self (-> (mid 1) (.map inc) (.unwrap)) 2)
    (.assertEqual self (-> (alet [a (mid 1)
                                  b (mid 2)
                                  c (mid 3)]
                             (+ a b c))
                           (.unwrap))
                  6)
    (.assertEqual self (-> (mlet [a (mid 1)
                                  b (mid (+ a 2))
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
                         (alet [a (mtry-on 1)
                                b (mtry-on 2)]
                           (+ a b)))
                  3)
    (with [_ (.assertRaises self RuntimeError)]
      (-> (alet [a (mtry-on 1)
                 b (mtry-on (raise RuntimeError))]
            (+ a b))
          (.unwrap))))

  (defn test-state [self]
    (.assertEqual self (.run (mlet [a (state.wrap "hello")
                                    b (state-on #((+ a " ") (inc s)))]
                               (state-on #((+ b "world") (inc s))))
                             1)
                  #("hello world" 3))
    (.assertEqual self (.run (mlet [greeting (reader-on (-get e "greeting" "hi"))]
                               (reader-on (+ greeting ", " (-get e "to" "world"))))
                             {"greeting" "hello" "to" "emacs"})
                  "hello, emacs")))

(export
  :objects [TestMonad])
