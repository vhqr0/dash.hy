(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.strtools :as s)

(defclass TestIter [TestCase]
  (defn test-iter [self]
    (.assertEqual self (ap-reduce (+ acc it) 0 (range 10)) 45)
    (.assertEqual self (ap-reduce (+ acc it) (range 10)) 45)
    (.assertEqual self (list (ap-map (inc it) (range 5))) [1 2 3 4 5])
    (.assertEqual self (list (eduction (map inc) (range 5))) [1 2 3 4 5])
    (.assertEqual self (list (ap-filter (even? it) (range 10))) [0 2 4 6 8])
    (.assertEqual self (list (eduction (filter even?) (range 10))) [0 2 4 6 8])
    (.assertEqual self (list (ap-remove (even? it) (range 10))) [1 3 5 7 9])
    (.assertEqual self (list (list (eduction (remove even?) (range 10)))) [1 3 5 7 9]))

  (defn test-cat [self]
    (.assertEqual self (list (concat-in [[1 2 3] [4 5] [6]])) [1 2 3 4 5 6])
    (.assertEqual self (list (concat [1 2 3] [4 5] [6])) [1 2 3 4 5 6])
    (.assertEqual self (list (ap-mapcat (range it) (range 5))) [0 0 1 0 1 2 0 1 2 3])
    (.assertEqual self (list (eduction (mapcat range) (range 5))) [0 0 1 0 1 2 0 1 2 3]))

  (defn test-pred [self]
    (.assertTrue self (ap-any? (even? it) [2 4 5]))
    (.assertFalse self (ap-any? (even? it) [1 3 5]))
    (.assertTrue self (ap-all? (even? it) [2 4 6]))
    (.assertFalse self (ap-all? (even? it) [1 3 4]))))

(defclass TestIterPart [TestCase]
  (defn test-nth [self]
    (.assertEqual self (nth [5 6 7 8 9 10] 2) 7)
    (.assertEqual self (nth (range 5 10) 2) 7)
    (with [_ (.assertRaises self IndexError)]
      (nth (range 5 10) 10)))

  (defn test-take [self]
    (.assertEqual self (list (take 3 (range 5))) [0 1 2])
    (.assertEqual self (list (eduction (take 3) (range 5))) [0 1 2])
    (.assertEqual self (list (take 10 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (eduction (take 10) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (take 0 (range 5))) [])
    (.assertEqual self (list (eduction (take 0) (range 5))) [])
    (.assertEqual self (list (drop 3 (range 5))) [3 4])
    (.assertEqual self (list (eduction (drop 3) (range 5))) [3 4])
    (.assertEqual self (list (drop 10 (range 5))) [])
    (.assertEqual self (list (eduction (drop 10) (range 5))) [])
    (.assertEqual self (list (drop 0 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (eduction (drop 0) (range 5))) [0 1 2 3 4])
    (let [#(take drop) (split-at 3 (range 5))]
      (.assertEqual self take [0 1 2])
      (.assertEqual self (list drop) [3 4]))
    (let [#(take drop) (split-at 10 (range 5))]
      (.assertEqual self take [0 1 2 3 4])
      (.assertEqual self (list drop) []))
    (let [#(take drop) (split-at 0 (range 5))]
      (.assertEqual self take [])
      (.assertEqual self (list drop) [0 1 2 3 4]))
    (.assertEqual self (list (ap-take-while (<= it 2) (range 5))) [0 1 2])
    (.assertEqual self (list (eduction (ap-take-while (<= it 2)) (range 5))) [0 1 2])
    (.assertEqual self (list (ap-take-while (<= it 5) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (eduction (ap-take-while (<= it 5)) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (ap-take-while (>= it 5) (range 5))) [])
    (.assertEqual self (list (eduction (ap-take-while (>= it 5)) (range 5))) [])
    (.assertEqual self (list (ap-drop-while (<= it 2) (range 5))) [3 4])
    (.assertEqual self (list (eduction (ap-drop-while (<= it 2)) (range 5))) [3 4])
    (.assertEqual self (list (ap-drop-while (<= it 5) (range 5))) [])
    (.assertEqual self (list (eduction (ap-drop-while (<= it 5)) (range 5))) [])
    (.assertEqual self (list (ap-drop-while (>= it 5) (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (eduction (ap-drop-while (>= it 5)) (range 5))) [0 1 2 3 4])
    (let [#(take drop) (ap-split-with (<= it 2) (range 5))]
      (.assertEqual self take [0 1 2])
      (.assertEqual self (list drop) [3 4]))
    (let [#(take drop) (ap-split-with (<= it 5) (range 5))]
      (.assertEqual self take [0 1 2 3 4])
      (.assertEqual self (list drop) []))
    (let [#(take drop) (ap-split-with (>= it 5) (range 5))]
      (.assertEqual self take [])
      (.assertEqual self (list drop) [0 1 2 3 4]))
    (.assertEqual self (list (take-nth 3 (range 10))) [0 3 6 9])
    (.assertEqual self (list (eduction (take-nth 3) (range 10))) [0 3 6 9])
    (.assertEqual self (list (take-nth 1 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (eduction (take-nth 1) (range 5))) [0 1 2 3 4]))

  (defn test-last [self]
    (.assertEqual self (last (range 5)) 4)
    (.assertEqual self (last (range 0)) None)
    (.assertEqual self (list (butlast (range 5))) [0 1 2 3])
    (.assertEqual self (list (butlast (range 0))) [])
    (.assertEqual self (list (take-last 3 (range 5))) [2 3 4])
    (.assertEqual self (list (take-last 10 (range 5))) [0 1 2 3 4])
    (.assertEqual self (list (take-last 0 (range 5))) [])
    (.assertEqual self (list (drop-last 3 (range 5))) [0 1])
    (.assertEqual self (list (drop-last 10 (range 5))) [])
    (.assertEqual self (list (drop-last 0 (range 5))) [0 1 2 3 4]))

  (defn test-window [self]
    (.assertEqual self (list (map first (window (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (map first (sized-window 3 (range 5)))) [0 1 2])
    (.assertEqual self (list (map first (sized-window 1 (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (map first (sized-window 10 (range 5)))) [])
    (.assertEqual self (list (map first (sized-window-all 3 (range 5)))) [0 1 2])
    (.assertEqual self (list (map first (sized-window-all 1 (range 5)))) [0 1 2 3 4])
    (.assertEqual self (list (map first (sized-window-all 10 (range 5)))) [0]))

  (defn test-partition [self]
    (.assertEqual self (list (partition 3 (range 10))) [[0 1 2] [3 4 5] [6 7 8]])
    (.assertEqual self (list (eduction (partition 3) (range 10))) [[0 1 2] [3 4 5] [6 7 8]])
    (.assertEqual self (list (partition 3 2 (range 10))) [[0 1 2] [2 3 4] [4 5 6] [6 7 8]])
    (.assertEqual self (list (partition-all 3 (range 10))) [[0 1 2] [3 4 5] [6 7 8] [9]])
    (.assertEqual self (list (eduction (partition-all 3) (range 10))) [[0 1 2] [3 4 5] [6 7 8] [9]])
    (.assertEqual self (list (partition-all 3 2 (range 10))) [[0 1 2] [2 3 4] [4 5 6] [6 7 8] [8 9]])
    (.assertEqual self (list (ap-partition-by (even? it) [2 4 5 6 8])) [[2 4] [5] [6 8]])))

(defclass TestIterMisc [TestCase]
  (defn test-iterate [self]
    (.assertEqual self (list (take 5 (ap-iterate (inc it) 0))) [0 1 2 3 4])
    (.assertEqual self (list (ap-iterate 5 (inc it) 0)) [0 1 2 3 4])
    (.assertEqual self (list (take 5 (range 10))) [0 1 2 3 4]))

  (defn test-repeat [self]
    (.assertEqual self (list (take 3 (repeat 8))) [8 8 8])
    (.assertEqual self (list (repeat 3 8)) [8 8 8])
    (.assertEqual self (list (take 3 (let [acc (list)]
                                       (ap-repeatedly (len (conj! acc (len acc)))))))
                  [1 2 3])
    (.assertEqual self (list (let [acc (list)]
                               (ap-repeatedly 3 (len (conj! acc (len acc))))))
                  [1 2 3])
    (.assertEqual self (list (take 5 (cycle (iter (range 3))))) [0 1 2 0 1])
    (.assertEqual self (list (cycle 2 (iter (range 3)))) [0 1 2 0 1 2]))

  (defn test-interleave [self]
    (.assertEqual self (list (interleave (range 5) (range 5 8) (range 10 12))) [0 5 10 1 6 11])
    (.assertEqual self (s.concats-in (interpose ":" (map str (range 5)))) "0:1:2:3:4")
    (.assertEqual self (s.concats-in (eduction (comp (interpose ":") (map str)) (range 5))) "0:1:2:3:4")
    (.assertEqual self (s.concats-in (interpose ":" (range 0))) "")
    (.assertEqual self (s.concats-in (eduction (interpose ":") (range 0))) ""))

  (defn test-keep [self]
    (.assertEqual self (list (ap-keep (-get it ':a) [{:a 1} {:b 2} {:a 3 :b 4}])) [1 3])
    (.assertEqual self (list (eduction (ap-keep (-get it ':a)) [{:a 1} {:b 2} {:a 3 :b 4}])) [1 3])
    (.assertIsNone self (ap-some (when (even? it) (inc it)) [1 3 5]))
    (.assertEqual self (ap-some (when (even? it) (inc it)) [2 4 5]) 3))

  (defn test-group [self]
    (.assertEqual self (ap-group-by (even? it) (range 10)) {True [0 2 4 6 8] False [1 3 5 7 9]}))

  (defn test-trans [self]
    (.assertEqual self (list (replace {0 "N/A"} [1 0 2 0 3])) [1 "N/A" 2 "N/A" 3])
    (.assertEqual self (list (eduction (replace {0 "N/A"}) [1 0 2 0 3])) [1 "N/A" 2 "N/A" 3])
    (.assertEqual self (list (distinct [1 0 2 0 3])) [1 0 2 3])
    (.assertEqual self (list (eduction (distinct) [1 0 2 0 3])) [1 0 2 3])
    (.assertEqual self (list (dedupe [1 0 0 2 2 2 0 0 0 0 3 3 3 3 3])) [1 0 2 0 3])
    (.assertEqual self (list (eduction (dedupe) [1 0 0 2 2 2 0 0 0 0 3 3 3 3 3])) [1 0 2 0 3])
    (.assertEqual self (list (flatten [1 2 [3 4 [5 6]] [7 [8 [9]]]])) [1 2 3 4 5 6 7 8 9]))

  (defn test-complex [self]
    (.assertEqual self
                  (->> (range 20)
                       (filter even?)
                       (ap-map (range (// it 2)))
                       concat-in
                       (take 23)
                       (reduce + 0))
                  (transduce
                    (comp
                      (filter even?)
                      (ap-map (range (// it 2)))
                      cat
                      (take 23))
                    + 0 (range 20)))))

(export
  :objects [TestIter TestIterPart TestIterMisc])
