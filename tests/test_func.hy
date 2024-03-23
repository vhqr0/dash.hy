(require
  dash *)

(import
  unittest [TestCase]
  dash *)

(defclass TestFunc [TestCase]
  (defn test-curry [self]
    (let [c2 (curry 2)
          adder (c2 +)
          adder3 (adder 3)]
      (.assertEqual self (adder3 2) 5)
      (.assertEqual self (adder3 3) 6)
      (.assertEqual self (adder3 2 3) 8)))

  (defn test-comp [self]
    (.assertEqual self ((comp) 1) 1)
    (.assertEqual self ((comp inc) 1) 2)
    (.assertEqual self ((comp inc inc) 1) 3)
    (.assertEqual self ((comp inc (partial * 2) inc) 1) 5))

  (defn test-juxt [self]
    (.assertEqual self ((juxt inc dec) 2) #(3 1))
    (.assertEqual self ((juxt + *) 2 3) #(5 6)))

  (defn test-trampoline [self]
    (defn myeven? [i]
      (if (zero? i) True (fn [] (myodd? (dec i)))))
    (defn myodd? [i]
      (if (zero? i) False (fn [] (myeven? (dec i)))))
    (.assertTrue self (trampoline (myeven? 4)))
    (.assertFalse self (trampoline (myeven? 5))))

  (defn test-pred [self]
    (let [str-or-pos? (orfn (andfn int? pos?) str?)]
      (.assertTrue self (str-or-pos? "a"))
      (.assertTrue self (str-or-pos? 1))
      (.assertFalse self (str-or-pos? 1.0))
      (.assertFalse self (str-or-pos? 0))
      (.assertFalse self (str-or-pos? b"")))
    (let [not-str-or-bytes? (andfn (notfn str?) (notfn bytes?))]
      (.assertFalse self (not-str-or-bytes? "a"))
      (.assertFalse self (not-str-or-bytes? b"a"))
      (.assertTrue self (not-str-or-bytes? 1)))))

(export
  :objects [TestFunc])
