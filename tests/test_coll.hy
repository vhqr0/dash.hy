(require
  dash *)

(import
  unittest [TestCase]
  dash *
  operator :as o)

(defclass TestColl [TestCase]
  (defn test-get [self]
    (.assertTrue self (contains? [1 2 3] 0))
    (.assertFalse self (contains? [1 2 3] 3))
    (.assertTrue self (contains? {0 1 2 3} 0))
    (.assertFalse self (contains? {0 1 2 3} 3))
    (.assertTrue self (contains? #{0 1 2} 0))
    (.assertFalse self (contains? #{0 1 2} 3))
    (.assertEqual self (-get (seq (range 10)) 2) 2)
    (let [d [{"a" 1 "b" 2} {"c" #{3 4 5}}]]
      (.assertEqual self (-get-in d [1 "c" 4]) 4)
      (.assertEqual self (-get-in d [0 "c" 4] "?") "?")))

  (defn test-misc [self]
    (.assertEqual self (-cut (list (range 14)) 0 14 3) [0 3 6 9 12])
    (.assertEqual self (list (-cut (iter (range 14)) 0 14 3)) [0 3 6 9 12])
    (.assertEqual self (-len [1 2 3]) 3)
    (.assertEqual self (-len (iter [1 2 3])) 3)
    (.assertEqual self (list (-reversed (iter (range 3)))) [2 1 0]))

  (defn test-assoc [self]
    (let [d {"a" 1}]
      (.assertEqual self (assoc! d "b" 2) {"a" 1 "b" 2})
      (.assertEqual self (assoc! d "b" 3) {"a" 1 "b" 3})
      (.assertEqual self (dissoc! d "a") {"b" 3})
      (.assertEqual self (update! d "b" o.add 2) {"b" 5}))
    (let [l [1 2 3]]
      (.assertEqual self (assoc! l 1 3) [1 3 3])
      (.assertEqual self (dissoc! l 0) [3 3])
      (.assertEqual self (update! l 0 o.add 2) [5 3]))
    (let [d [{"a" 1 "b" 2} {"c" 3}]]
      (.assertEqual self (assoc-in! d [1 "d"] 4) [{"a" 1 "b" 2} {"c" 3 "d" 4}])
      (.assertEqual self (assoc-in! d [0 "b"] 3) [{"a" 1 "b" 3} {"c" 3 "d" 4}])
      (.assertEqual self (dissoc-in! d [0 "a"]) [{"b" 3} {"c" 3 "d" 4}])
      (.assertEqual self (update-in! d [0 "b"] o.add 2) [{"b" 5} {"c" 3 "d" 4}])))

  (defn test-set [self]
    (let [s #{}]
      (.assertEqual self (into! s [1 2 3 1]) #{1 2 3})
      (.assertEqual self (conj! s 4) #{1 2 3 4})
      (.assertEqual self (disj! s 3) #{1 2 4})))

  (defn test-map [self]
    (let [d {}]
      (.assertEqual self (into! d [[1 2] [3 4]]) {1 2 3 4})
      (.assertEqual self (conj! d [5 6]) {1 2 3 4 5 6})))

  (defn test-sequence [self]
    (let [l []]
      (.assertEqual self (into! l [1 2 3]) [1 2 3])
      (.assertEqual self (conj! l 4) [1 2 3 4])
      (.assertEqual self (pop! l) [1 2 3])))

  (defn test-seq [self]
    (let [s (seq [1 2 3])]
      (.assertEqual self (list (into s [4 5])) [5 4 1 2 3])
      (.assertEqual self (list (conj s 4)) [4 1 2 3])
      (.assertEqual self (list (pop s)) [2 3])
      (.assertEqual self (peek s) 1)))

  (defn test-dict [self]
    (.assertEqual self (merge {1 2 3 4} [0 1 2]) {0 0 1 1 2 2 3 4})
    (.assertEqual self (ap-merge-with (+ acc it) {1 2 3 4} [0 1 2]) {0 0 1 3 2 2 3 4})))

(export
  :objects [TestColl])
