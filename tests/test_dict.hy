(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o)

(defclass TestDictGetSetDel [TestCase]
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

(defclass TestDictIter [TestCase]
  )

(defclass TestDictGetfn [TestCase]
  (defn test-collfn [self]
    (.assertEqual self ((-collfn #{1 2}) 1) True)
    (.assertEqual self ((-collfn #{1 2}) 3) False)
    (.assertEqual self ((-collfn [1 2]) 0) 1)
    (.assertIsNone self ((-collfn [1 2]) 2))
    (.assertEqual self ((-collfn {"a" 1 "b" 2}) "a") 1)
    (.assertIsNone self ((-collfn {"a" 1 "b" 2}) "c")))

  (defn test-juxt [self]
    (.assertEqual self ((-juxtv inc dec) 1) #(2 0))
    (.assertEqual self ((-juxtkw :a inc :b dec) 1) {"a" 2 "b" 0}))

  (defn test-keyfn [self]
    (.assertEqual self ((-juxtv-keyfn 0 2) [1 2 3]) #(1 3))
    (.assertEqual self ((-juxtkw-keyfn :a 0 :b 2) [1 2 3]) {"a" 1 "b" 3})))

(defclass TestDictOp [TestCase]
  )

(export
  :objects [TestDictGetSetDel TestDictIter TestDictGetfn TestDictOp])
