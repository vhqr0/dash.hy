(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o)

(defclass TestColl [TestCase]
  (defn test-get [self]
    (.assertTrue self (-contains? [1 2 3] 0))
    (.assertFalse self (-contains? [1 2 3] 3))
    (.assertTrue self (-contains? {0 1 2 3} 0))
    (.assertFalse self (-contains? {0 1 2 3} 3))
    (.assertTrue self (-contains? #{0 1 2} 0))
    (.assertFalse self (-contains? #{0 1 2} 3))
    (let [d [{"a" 1 "b" 2} {"c" #{3 4 5}}]]
      (.assertEqual self (-get-in d [1 "c" 4]) 4)
      (.assertEqual self (-get-in d [0 "c" 4] "?") "?")))

  (defn test-assoc [self]
    (let [d {"a" 1}]
      (.assertEqual self (-assoc! d "b" 2) {"a" 1 "b" 2})
      (.assertEqual self (-assoc! d "b" 3) {"a" 1 "b" 3})
      (.assertEqual self (-dissoc! d "a") {"b" 3})
      (.assertEqual self (-update! d "b" o.add 2) {"b" 5}))
    (let [l [1 2 3]]
      (.assertEqual self (-assoc! l 1 3) [1 3 3])
      (.assertEqual self (-dissoc! l 0) [3 3])
      (.assertEqual self (-update! l 0 o.add 2) [5 3])))

  (defn test-assoc-in [self]
    (let [d [{"a" 1 "b" 2} {"c" 3}]]
      (.assertEqual self (-assoc-in! d [1 "d"] 4) [{"a" 1 "b" 2} {"c" 3 "d" 4}])
      (.assertEqual self (-assoc-in! d [0 "b"] 3) [{"a" 1 "b" 3} {"c" 3 "d" 4}])
      (.assertEqual self (-dissoc-in! d [0 "a"]) [{"b" 3} {"c" 3 "d" 4}])
      (.assertEqual self (-update-in! d [0 "b"] o.add 2) [{"b" 5} {"c" 3 "d" 4}]))))

(defclass TestCollOp [TestCase]
  (defn test-set [self]
    (let [s #{}]
      (.assertEqual self (-into! s [1 2 3 1]) #{1 2 3})
      (.assertEqual self (-conj! s 4) #{1 2 3 4})
      (.assertEqual self (-disj! s 3) #{1 2 4})
      (.assertEqual self (-empty s) #{})))

  (defn test-map [self]
    (let [d {}]
      (.assertEqual self (-into! d [[1 2] [3 4]]) {1 2 3 4})
      (.assertEqual self (-conj! d [5 6]) {1 2 3 4 5 6})
      (.assertEqual self (-empty d) {})))

  (defn test-sequence [self]
    (let [l []]
      (.assertEqual self (-into! l [1 2 3]) [1 2 3])
      (.assertEqual self (-conj! l 4) [1 2 3 4])
      (.assertEqual self (-pop! l) [1 2 3])
      (.assertEqual self (-empty l) [])))

  (defn test-seq [self]
    (let [s (seq [1 2 3])]
      (.assertEqual self (list (-empty s)) [])
      (.assertEqual self (list (-into s [4 5])) [5 4 1 2 3])
      (.assertEqual self (list (-conj s 4)) [4 1 2 3])
      (.assertEqual self (list (-pop s)) [2 3])
      (.assertEqual self (-peek s) 1))))

(defclass TestDict [TestCase]
  (defn test-reduce [self]
    (.assertEqual self (--reduce-items {"1" 2 "3" 4} (+ acc (int k) v) 0) 10)
    (.assertEqual self (--reduce-keys {"1" 2 "3" 4} (+ acc (int k)) 0) 4)
    (.assertEqual self (--reduce-vals {"1" 2 "3" 4} (+ acc v) 0) 6))

  (defn test-map [self]
    (.assertEqual self (--map-items {1 2 3 4} #((+ k v) v)) {3 2 7 4})
    (.assertEqual self (--map-keys {1 2 3 4} (inc k)) {2 2 4 4})
    (.assertEqual self (--map-vals {1 2 3 4} (inc v)) {1 3 3 5}))

  (defn test-filter [self]
    (.assertEqual self (--filter-items {1 2 :k "v"} (and (keyword? k) (str? v))) {:k "v"})
    (.assertEqual self (--filter-keys {1 2 :k "v"} (int? k)) {1 2})
    (.assertEqual self (--filter-vals {1 2 :k "v"} (int? v)) {1 2}))

  (defn test-misc [self]
    (.assertEqual self (-select-keys {1 2 :k "v"} [1 3] :default 404) {1 2 3 404})
    (.assertEqual self (-merge {1 2 3 4} [0 1 2]) {0 0 1 1 2 2 3 4})
    (.assertEqual self (--merge-with (+ acc it) {1 2 3 4} [0 1 2]) {0 0 1 3 2 2 3 4})))

(export
  :objects [TestColl TestCollOp TestDict])
