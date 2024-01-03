(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o
  dash.strtools :as s)

(defclass TestIterReduceMapFilter [TestCase]
  (defn test-each [self]
    (.assertEqual self (let [acc [[1] [2 1] [3 2 1]]] (-each acc (fn [it] (-conj! it 0))) acc)
                  [[1 0] [2 1 0] [3 2 1 0]])
    (.assertEqual self (let [acc [[1] [2] [3]]]
                         (-each-indexed acc (fn [it-index it] (-conj! it it-index))) acc)
                  [[1 0] [2 1] [3 2]])
    (.assertEqual self (let [acc (list)] (-dotimes 5 (fn [it] (-conj! acc it))) acc)
                  [0 1 2 3 4]))

  (defn test-reduce [self]
    (.assertEqual self (-reduce-from o.add 0 (range 10)) 45)
    (.assertEqual self (-reduce o.add (range 10)) 45)
    (.assertEqual self (list (-reductions-from o.add 0 (range 5))) [0 0 1 3 6 10])
    (.assertEqual self (list (-reductions o.add (range 5))) [0 1 3 6 10]))

  (defn test-map [self]
    (.assertEqual self (list (--map (inc it) (range 5))) [1 2 3 4 5])
    (.assertEqual self (list (--map-indexed (-argv it-index it) (range 5 10)))
                  [#(0 5) #(1 6) #(2 7) #(3 8) #(4 9)])
    (.assertEqual self (list (--map-unzipped (+ #* them) (-zip (range 5) (range 5 10) (range 10 15))))
                  [15 18 21 24 27]))

  (defn test-filter [self]
    (.assertEqual self (list (--filter (even? it) (range 10))) [0 2 4 6 8])
    (.assertEqual self (list (--remove (even? it) (range 10))) [1 3 5 7 9]))

  (defn test-mapcat [self]
    (.assertEqual self (list (--mapcat (range it) (range 5))) [0 0 1 0 1 2 0 1 2 3])
    (.assertEqual self (list (--mapcat-indexed (-argv it-index it) (range 5 10)))
                  [0 5 1 6 2 7 3 8 4 9]))

  (defn test-mapcons [self]
    (.assertEqual self (list (--mapcons (even? it) (range 5)))
                  [(cons True 0) (cons False 1) (cons True 2) (cons False 3) (cons True 4)])
    (.assertEqual self (list (--mapcons-indexed (inc it-index) (range 5)))
                  [(cons 1 0) (cons 2 1) (cons 3 2) (cons 4 3) (cons 5 4)]))

  (defn test-keep [self]
    (.assertEqual self (list (--keep (-get it ':a) [{:a 1} {:b 2} {:a 3 :b 4}])) [1 3])
    (.assertEqual self (list (--keep-indexed (-get it it-index) [[0] [1] [2 3 4]])) [0 4]))

  (defn test-pred [self]
    (.assertIsNone self (--some (when (even? it) (inc it)) [1 3 5]))
    (.assertEqual self (--some (when (even? it) (inc it)) [2 4 5]) 3)
    (.assertIsNone self (--every (when (even? it) (inc it)) [2 4 5]))
    (.assertEqual self (--every (when (even? it) (inc it)) [2 4 6]) 7)
    (.assertTrue self (--any? (even? it) [2 4 5]))
    (.assertFalse self (--any? (even? it) [1 3 5]))
    (.assertTrue self (--all? (even? it) [2 4 6]))
    (.assertFalse self (--all? (even? it) [1 3 4]))
    (.assertTrue self (--not-any? (even? it) [1 3 5]))
    (.assertFalse self (--not-any? (even? it) [2 4 4]))
    (.assertTrue self (--not-all? (even? it) [2 4 5]))
    (.assertFalse self (--not-all? (even? it) [2 4 6]))))

(defclass TestIterGen [TestCase]
  (defn test-iterate [self]
    (.assertEqual self (list (-take 5 (--iterate (inc it) 0))) [0 1 2 3 4])
    (.assertEqual self (list (--iterate-n 5 (inc it) 0)) [0 1 2 3 4])
    (.assertEqual self (list (-take 5 (-range))) [0 1 2 3 4]))

  (defn test-repeat [self]
    (.assertEqual self (list (-take 3 (-repeat 8))) [8 8 8])
    (.assertEqual self (list (-repeat-n 3 8)) [8 8 8])
    (.assertEqual self (list (-take 3 (let [acc (list)]
                                        (--repeatedly (len (-conj! acc (len acc)))))))
                  [1 2 3])
    (.assertEqual self (list (let [acc (list)]
                               (--repeatedly-n 3 (len (-conj! acc (len acc))))))
                  [1 2 3])
    (.assertEqual self (list (-take 5 (-cycle (iter (range 3))))) [0 1 2 0 1])
    (.assertEqual self (list (-cycle-n 2 (iter (range 3)))) [0 1 2 0 1 2])))

(defclass TestIterMux [TestCase]
  (defn test-concat [self]
    (.assertEqual self (list (-concat-in [[1 2 3] [4 5] [6]])) [1 2 3 4 5 6])
    (.assertEqual self (list (-concat [1 2 3] [4 5] [6])) [1 2 3 4 5 6]))

  (defn test-zip [self]
    (.assertEqual self (list (-zip (range 5) (range 5 8) (range 10 12))) [[0 5 10] [1 6 11]])
    (.assertEqual self (list (-zip)) [])
    (.assertEqual self (list (-zip-fill "N/A" (range 5) (range 5 8)))
                  [[0 5] [1 6] [2 7] [3 "N/A"] [4 "N/A"]])
    (.assertEqual self (list (-zip-fill "N/A")) []))

  (defn test-tee [self]
    (let [#(it1 it2) (-tee-n 2 (iter (range 5)))]
      (.assertEqual self (list it1) [0 1 2 3 4])
      (.assertEqual self (list it2) [0 1 2 3 4])
      (.assertEqual self (list it1) [])
      (.assertEqual self (list it2) []))

    (let [its (-tee (iter (range 5)))
          #(it1 it2) #((next its) (next its))]
      (.assertEqual self (list it1) [0 1 2 3 4])
      (.assertEqual self (list it2) [0 1 2 3 4])
      (.assertEqual self (list it1) [])
      (.assertEqual self (list it2) [])
      (.assertEqual self (list (next its)) [0 1 2 3 4])))

  (defn test-interleave [self]
    (.assertEqual self (list (-interleave (range 5) (range 5 8) (range 10 12)))
                  [0 5 10 1 6 11])
    (.assertEqual self (list (-interleave-fill "N/A" (range 5) (range 5 8)))
                  [0 5 1 6 2 7 3 "N/A" 4 "N/A"])
    (.assertEqual self (s.concat-str-in (-interpose ":" (-map str (range 5))))
                  "0:1:2:3:4")
    (.assertEqual self (s.concat-str-in (-interpose ":" (range 0))) "")))

(defclass TestIterPart [TestCase]
  (defn test-take [self]
    (.assertEqual self (list (-take 3 (range 5))) [0 1 2])
    (.assertEqual self (list (-take 10 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (-take 0 (range 5))) [])
    (.assertEqual self (list (-drop 3 (range 5))) [3 4])
    (.assertEqual self (list (-drop 10 (range 5))) [])
    (.assertEqual self (list (-drop 0 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (--take-while (<= it 2) (range 5))) [0 1 2])
    (.assertEqual self (list (--take-while (<= it 5) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (--take-while (>= it 5) (range 5))) [])
    (.assertEqual self (list (--drop-while (<= it 2) (range 5))) [3 4])
    (.assertEqual self (list (--drop-while (<= it 5) (range 5))) [])
    (.assertEqual self (list (--drop-while (>= it 5) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (-take-nth 3 (range 10))) [0 3 6 9])
    (.assertEqual self (list (-take-nth 1 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (-drop-nth 3 (range 10))) [1 2 4 5 7 8])
    (.assertEqual self (list (-drop-nth 1 (range 5))) []))

  (defn test-window [self]
    (.assertEqual self (list (-map first (-unsized-window (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (-map first (-sized-window 3 (range 5)))) [0 1 2])
    (.assertEqual self (list (-map first (-sized-window 1 (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (-map first (-sized-window 10 (range 5)))) [])
    (.assertEqual self (list (-map first (-sized-loose-window 3 (range 5)))) [0 1 2])
    (.assertEqual self (list (-map first (-sized-loose-window 1 (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (-map first (-sized-loose-window 10 (range 5)))) [0]))

  (defn test-last [self]
    (.assertEqual self (-last (range 5)) 4)
    (.assertEqual self (-last (range 0)) None)
    (.assertEqual self (list (-butlast (range 5))) [0 1 2 3])
    (.assertEqual self (list (-butlast (range 0))) [])
    (.assertEqual self (list (-take-last 3 (range 5))) [2 3 4])
    (.assertEqual self (list (-take-last 10 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (-take-last 0 (range 5))) [])
    (.assertEqual self (list (-drop-last 3 (range 5))) [0 1])
    (.assertEqual self (list (-drop-last 10 (range 5))) [])
    (.assertEqual self (list (-drop-last 0 (range 5))) [0 1 2 3 4]))

  (defn test-split [self]
    (let [#(take drop) (-split-at 3 (range 5))]
      (.assertEqual self take [0 1 2])
      (.assertEqual self (list drop) [3 4]))
    (let [#(take drop) (-split-at 10 (range 5))]
      (.assertEqual self take [0 1 2 3 4])
      (.assertEqual self (list drop) []))
    (let [#(take drop) (-split-at 0 (range 5))]
      (.assertEqual self take [])
      (.assertEqual self (list drop) [0 1 2 3 4]))
    (let [#(take drop) (--split-with (<= it 2) (range 5))]
      (.assertEqual self take [0 1 2])
      (.assertEqual self (list drop) [3 4]))
    (let [#(take drop) (--split-with (<= it 5) (range 5))]
      (.assertEqual self take [0 1 2 3 4])
      (.assertEqual self (list drop) []))
    (let [#(take drop) (--split-with (>= it 5) (range 5))]
      (.assertEqual self take [])
      (.assertEqual self (list drop) [0 1 2 3 4])))

  (defn test-partition [self]
    (.assertEqual self (list (-partition 3 (range 10))) [[0 1 2] [3 4 5] [6 7 8]])
    (.assertEqual self (list (-partition-all 3 (range 10))) [[0 1 2] [3 4 5] [6 7 8] [9]])
    (.assertEqual self (list (-partition-step 3 2 (range 10)))
                  [[0 1 2] [2 3 4] [4 5 6] [6 7 8]])
    (.assertEqual self (list (-partition-all-step 3 2 (range 10)))
                  [[0 1 2] [2 3 4] [4 5 6] [6 7 8] [8 9]])
    (.assertEqual self (list (--partition-by (even? it) [2 4 5 6 8])) [[2 4] [5] [6 8]])))

(defclass TestIterOp [TestCase]
  (defn test-count [self]
    (.assertEqual self (-count [5 6 7 8 9 10]) 6)
    (.assertEqual self (-count (range 10)) 10))

  (defn test-nth [self]
    (.assertEqual self (-nth [5 6 7 8 9 10] 2) 7)
    (.assertEqual self (-nth (range 5 10) 2) 7)
    (with [_ (.assertRaises self IndexError)]
      (-nth (range 5 10) 10))))

(defclass TestIterMisc [TestCase]
  (defn test-trans [self]
    (.assertEqual self (list (-replace {0 "N/A"} [1 0 2 0 3])) [1 "N/A" 2 "N/A" 3])
    (.assertEqual self (list (-distinct [1 0 2 0 3])) [1 0 2 3])
    (.assertEqual self (list (-dedupe [1 0 0 2 2 2 0 0 0 0 3 3 3 3 3])) [1 0 2 0 3]))

  (defn test-group [self]
    (.assertEqual self (--group-by (even? it) (range 10)) {True [0 2 4 6 8] False [1 3 5 7 9]})))

(export
  :objects [TestIterReduceMapFilter TestIterGen TestIterMux TestIterPart TestIterOp TestIterMisc])
