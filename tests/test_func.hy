(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.operator :as o)

(defclass TestFuncCall [TestCase]
  (defn test-arg [self]
    (.assertEqual self (-argv 1 2 3) #(1 2 3))
    (.assertEqual self (-argkw :a 1 :b 2) {"a" 1 "b" 2})
    (.assertEqual self (-arg 1 2 3 :a 1 :b 2) #(#(1 2 3) {"a" 1 "b" 2})))

  (defn test-apply [self]
    (.assertEqual self (-applyv -arg [1 2 3]) #(#(1 2 3) {}))
    (.assertEqual self (-applykw -arg {"a" 1 "b" 2}) #(#() {"a" 1 "b" 2}))
    (.assertEqual self (-apply -arg [1 2 3] {"a" 1 "b" 2}) #(#(1 2 3) {"a" 1 "b" 2}))
    (.assertEqual self (-funcall -arg 1 2 3 :a 1 :b 2) #(#(1 2 3) {"a" 1 "b" 2})))

  (defn test-trampoline [self]
    (defn myeven? [i]
      (if (zero? i) True (fn [] (myodd? (dec i)))))
    (defn myodd? [i]
      (if (zero? i) False (fn [] (myeven? (dec i)))))
    (.assertTrue self (-trampoline (myeven? 4)))
    (.assertFalse self (-trampoline (myeven? 5)))))

(defclass TestFuncOp [TestCase]
  (defn test-partial [self]
    (.assertEqual self ((-partial -arg 1 2 :a 1) 3 :b 2) #(#(1 2 3) {"a" 1 "b" 2}))
    (.assertEqual self ((-rpartial -arg 3 :b 2) 1 2 :a 1) #(#(1 2 3) {"a" 1 "b" 2})))

  (defn test-comp [self]
    (.assertEqual self ((-comp) 1) 1)
    (.assertEqual self ((-comp inc) 1) 2)
    (.assertEqual self ((-comp inc inc) 1) 3))

  (defn test-curry [self]
    (let [c2 (-curry 2)
          adder (c2 o.add)
          adder3 (adder 3)]
      (.assertEqual self (adder3 2) 5)
      (.assertEqual self (adder3 3) 6)
      (.assertEqual self (adder3 2 3) 8)))

  (defn test-pred [self]
    (let [str-or-pos? (-orfn (-andfn int? pos?) str?)]
      (.assertTrue self (str-or-pos? "a"))
      (.assertTrue self (str-or-pos? 1))
      (.assertFalse self (str-or-pos? 1.0))
      (.assertFalse self (str-or-pos? 0))
      (.assertFalse self (str-or-pos? b"")))
    (let [not-str-or-bytes? (-andfn (-notfn str?) (-notfn bytes?))]
      (.assertFalse self (not-str-or-bytes? "a"))
      (.assertFalse self (not-str-or-bytes? b"a"))
      (.assertTrue self (not-str-or-bytes? 1))))

  (defn test-juxt [self]
    (.assertEqual self ((-juxtv inc dec) 1) #(2 0))
    (.assertEqual self ((-juxtkw :a inc :b dec) 1) {"a" 2 "b" 0})
    (.assertEqual self ((--juxtv (+ it 2) (* it 2)) 3) #(5 6))
    (.assertEqual self ((--juxtkw :a (+ it 2) :b (* it 2)) 3) {"a" 5 "b" 6})))

(export
  :objects [TestFuncCall TestFuncOp])
