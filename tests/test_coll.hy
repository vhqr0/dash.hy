(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o)

(defclass TestItem [TestCase]
  (defn test-impure [self]
    (let [d {"a" 1}]
      (-assoc! d "b" 2)
      (.assertEqual self d {"a" 1 "b" 2})
      (-assoc! d "b" 3)
      (.assertEqual self d {"a" 1 "b" 3})
      (-dissoc! d "a")
      (.assertEqual self d {"b" 3})
      (-update! d "b" o.add 2)
      (.assertEqual self d {"b" 5}))
    (let [l [1 2 3]]
      (-assoc! l 1 3)
      (.assertEqual self l [1 3 3])
      (-dissoc! l 0)
      (.assertEqual self l [3 3])
      (-update! l 0 o.add 2)
      (.assertEqual self l [5 3])))

  (defn test-pure [self]
    (let [d {"a" 1 "b" 2}]
      (.assertEqual self (-assoc d "b" 3) {"a" 1 "b" 3})
      (.assertEqual self (-dissoc d "a") {"b" 2})
      (.assertEqual self (-update d "b" o.add 2) {"a" 1 "b" 4})
      (.assertEqual self d {"a" 1 "b" 2}))
    (let [l [1 2 3]]
      (.assertEqual self (-assoc l 1 3) [1 3 3])
      (.assertEqual self (-dissoc l 0) [2 3])
      (.assertEqual self (-update l 0 o.add 2) [3 2 3])
      (.assertEqual self l [1 2 3])))

  (defn test-impure-in [self]
    (let [d [{"a" 1 "b" 2} {"c" 3}]]
      (-assoc-in! d [1 "d"] 4)
      (.assertEqual self d [{"a" 1 "b" 2} {"c" 3 "d" 4}])
      (-assoc-in! d [0 "b"] 3)
      (.assertEqual self d [{"a" 1 "b" 3} {"c" 3 "d" 4}])
      (-dissoc-in! d [0 "a"])
      (.assertEqual self d [{"b" 3} {"c" 3 "d" 4}])
      (-update-in! d [0 "b"] o.add 2)
      (.assertEqual self d [{"b" 5} {"c" 3 "d" 4}])))

  (defn test-pure-in [self]
    (let [d [{"a" 1 "b" 2} {"c" 3 "d" 4}]]
      (.assertEqual self (-assoc-in d [1 "e"] 5) [{"a" 1 "b" 2} {"c" 3 "d" 4 "e" 5}])
      (.assertEqual self (-assoc-in d [0 "b"] 3) [{"a" 1 "b" 3} {"c" 3 "d" 4}])
      (.assertEqual self (-dissoc-in d [1 "d"]) [{"a" 1 "b" 2} {"c" 3}])
      (.assertEqual self (-update-in d [0 "a"] o.add 2) [{"a" 3 "b" 2} {"c" 3 "d" 4}])
      (.assertEqual self d [{"a" 1 "b" 2} {"c" 3 "d" 4}]))))

(defclass TestDictOp [TestCase]
  (defn test-reduce [self]
    (.assertEqual self (--reduce-items {"1" 2 "3" 4} (+ acc (int k) v) 0) 10))

  (defn test-map [self]
    (.assertEqual self (--map-items {1 2 3 4} #((+ k v) v)) {3 2 7 4})
    (.assertEqual self (--map-keys {1 2 3 4} (inc it)) {2 2 4 4})
    (.assertEqual self (--map-vals {1 2 3 4} (inc it)) {1 3 3 5}))

  (defn test-filter [self]
    (.assertEqual self (--filter-items {1 2 :k "v"} (and (keyword? k) (str? v))) {:k "v"})
    (.assertEqual self (--filter-keys {1 2 :k "v"} (int? it)) {1 2})
    (.assertEqual self (--filter-keys {1 2 :k "v"} (int? it)) {1 2}))

  (defn test-misc [self]
    (.assertEqual self (-select-keys {1 2 :k "v"} [1 3]) {1 2 3 None})
    (.assertEqual self (-merge {1 2 3 4} [0 1 2]) {0 0 1 1 2 2 3 4})
    (.assertEqual self (--merge-with (+ acc it) {1 2 3 4} [0 1 2]) {0 0 1 3 2 2 3 4})))

(defclass TestCollGet [TestCase]
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

  (defn test-collfn [self]
    (.assertEqual self ((-collfn #{1 2}) 1) 1)
    (.assertEqual self ((-collfn #{1 2}) 3) None)
    (.assertEqual self ((-collfn [1 2]) 0) 1)
    (.assertIsNone self ((-collfn [1 2]) 2))
    (.assertEqual self ((-collfn {"a" 1 "b" 2}) "a") 1)
    (.assertIsNone self ((-collfn {"a" 1 "b" 2}) "c")))

  (defn test-juxt [self]
    (.assertEqual self ((-juxtv inc dec) 1) #(2 0))
    (.assertEqual self ((-juxtkw :a inc :b dec) 1) {"a" 2 "b" 0})
    (.assertEqual self ((--juxtv (+ it 2) (* it 2)) 3) #(5 6))
    (.assertEqual self ((--juxtkw :a (+ it 2) :b (* it 2)) 3) {"a" 5 "b" 6}))

  (defn test-keyfn [self]
    (.assertEqual self ((-juxtv-keyfn 0 2) [1 2 3]) #(1 3))
    (.assertEqual self ((-juxtkw-keyfn :a 0 :b 2) [1 2 3]) {"a" 1 "b" 3})))

(defclass TestCollOp [TestCase]
  (defn test-set [self]
    (let [s #{1 2 3}]
      (-empty! s)
      (.assertEqual self s #{})
      (-into! s [1 2 3 1])
      (.assertEqual self s #{1 2 3})
      (-conj! s 4)
      (.assertEqual self s #{1 2 3 4})
      (-disj! s 3)
      (.assertEqual self s #{1 2 4})
      (.assertEqual self (-empty s) #{})
      (.assertEqual self (-into s [3 5]) #{1 2 3 4 5})
      (.assertEqual self (-conj s 3) #{1 2 3 4})
      (.assertEqual self (-disj s 2) #{1 4})
      (.assertEqual self s #{1 2 4})))

  (defn test-map [self]
    (let [d {1 2 3 4}]
      (-empty! d)
      (.assertEqual self d {})
      (-into! d [[1 2] [3 4]])
      (.assertEqual self d {1 2 3 4})
      (-conj! d [5 6])
      (.assertEqual self d {1 2 3 4 5 6})
      (.assertEqual self (-empty d) {})
      (.assertEqual self (-into d [[7 8] [9 10]]) {1 2 3 4 5 6 7 8 9 10})
      (.assertEqual self (-conj d [7 8]) {1 2 3 4 5 6 7 8})
      (.assertEqual self d {1 2 3 4 5 6})))

  (defn test-sequence [self]
    (let [l [1 2 3]]
      (-empty! l)
      (.assertEqual self l [])
      (-into! l [1 2 3])
      (.assertEqual self l [1 2 3])
      (-conj! l 4)
      (.assertEqual self l [1 2 3 4])
      (-pop! l)
      (.assertEqual self l [1 2 3])
      (.assertEqual self (-empty l) [])
      (.assertEqual self (-into l [4 5]) [1 2 3 4 5])
      (.assertEqual self (-conj l 4) [1 2 3 4])
      (.assertEqual self (-pop l) [1 2])
      (.assertEqual self (-peek l) 3)
      (.assertEqual self l [1 2 3])))

  (defn test-seq [self]
    (let [s (seq [1 2 3])]
      (.assertEqual self (list (-empty s)) [])
      (.assertEqual self (list (-into s [4 5])) [5 4 1 2 3])
      (.assertEqual self (list (-conj s 4)) [4 1 2 3])
      (.assertEqual self (list (-pop s)) [2 3])
      (.assertEqual self (-peek s) 1))))

(export
  :objects [TestItem TestDictOp TestCollGet TestCollOp])
