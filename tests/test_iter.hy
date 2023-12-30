(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o
  dash.strtools :as s)

(defclass TestReduceMapFilter [TestCase]
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
    (.assertEqual self (list (-map* o.add (-zip (range 5) (range 5 10) (range 10 15))))
                  [15 18 21 24 27])
    (.assertEqual self (list (--map (inc it) (range 5))) [1 2 3 4 5])
    (.assertEqual self (list (--map-indexed (-args it-index it) (range 5 10)))
                  [#(0 5) #(1 6) #(2 7) #(3 8) #(4 9)]))

  (defn test-filter [self]
    (.assertEqual self (list (--filter (even? it) (range 10))) [0 2 4 6 8])
    (.assertEqual self (list (--remove (even? it) (range 10))) [1 3 5 7 9]))

  (defn test-mapcat [self]
    (.assertEqual self (list (--mapcat (range it) (range 5))) [0 0 1 0 1 2 0 1 2 3])
    (.assertEqual self (list (--mapcat-indexed (-args it-index it) (range 5 10)))
                  [0 5 1 6 2 7 3 8 4 9]))

  (defn test-keep [self]
    (.assertEqual self (list (--keep (-get it ':a) [{:a 1} {:b 2} {:a 3 :b 4}])) [1 3])
    (.assertEqual self (list (--keep-indexed (-get it it-index) [[0] [1] [2 3 4]])) [0 4]))

  (defn test-annotate [self]
    (.assertEqual self (list (--annotate (inc it) (range 5)))
                  [#(1 0) #(2 1) #(3 2) #(4 3) #(5 4)])
    (.assertEqual self (list (--annotate-indexed (+ it-index it) (range 5 10)))
                  [#(5 5) #(7 6) #(9 7) #(11 8) #(13 9)]))

  (defn test-pred [self]
    (.assertIsNone self (--some (when (even? it) (inc it)) [1 3 5]))
    (.assertEqual self (--some (when (even? it) (inc it)) [2 4 5]) 3)
    (.assertTrue self (--any? (even? it) [2 4 5]))
    (.assertFalse self (--any? (even? it) [1 3 5]))
    (.assertTrue self (--all? (even? it) [2 4 6]))
    (.assertFalse self (--all? (even? it) [1 3 4]))))

(defclass TestIterOp [TestCase]
  (defn test-concat [self]
    (.assertEqual self (list (-concat-in [[1 2 3] [4 5] [6]])) [1 2 3 4 5 6])
    (.assertEqual self (list (-concat [1 2 3] [4 5] [6])) [1 2 3 4 5 6])
    (.assertEqual self (list (-cons -1 (range 5))) [-1 0 1 2 3 4]))

  (defn test-pair [self]
    (.assertIsNone self (-iterpair (range 0)))
    (let [#(first rest) (-iterpair (range 5))]
      (.assertEqual self first 0)
      (.assertEqual self (list rest) [1 2 3 4]))
    (.assertTrue self (-empty? (range 0)))
    (.assertFalse self (-empty? (range 5)))
    (.assertIsNone self (-first (range 0)))
    (.assertIsNotNone self (-first (range 5)))
    (.assertIsNone self (-rest (range 0)))
    (.assertIsNotNone self (-rest (range 5)))))

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
