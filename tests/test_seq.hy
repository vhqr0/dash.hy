(require
  dash *)

(import
  unittest [TestCase]
  dash *
  dash.strtools :as s)

(defclass TestSeq [TestCase]
  (defn test-cons [self]
    (.assertEqual self (match (cons 1 2) (cons x y) #(x y)) #(1 2))
    (.assertEqual self (clist 1 2 3) (cons 1 (cons 2 (cons 3))))
    (.assertEqual self (clist-conj (cons 1) 2) (clist 2 1))
    (.assertEqual self (clist-into (cons 1) (range 3)) (clist 2 1 0 1))
    (.assertEqual self (list (clist-iter (clist 1 2 3))) [1 2 3])
    (.assertEqual self (list (clist-iter (clist-reverse (clist 1 2 3)))) [3 2 1]))

  (defn test-seq [self]
    (let [s (seq (cons 1 (range 3)))]
      (.assertIsInstance self s.data now)
      (.assertIsInstance self s.data.data cons)
      (.assertEqual self s.data.data.car 1)
      (setv s s.data.data.cdr)
      (.assertIsInstance self s.data later)
      (.assertEqual self (first s) 0)
      (.assertIsInstance self s.data now)
      (.assertEqual self (list s) [0 1 2])))

  (defn test-repr [self]
    (.assertEqual self (repr (cons 1)) "(1)")
    (.assertEqual self (repr (cons 1 2)) "(1 . 2)")
    (.assertEqual self (repr (cons 1 (cons 2 3))) "(1 2 . 3)")
    (.assertEqual self (repr (clist 1 2 3)) "(1 2 3)")
    (.assertEqual self (repr (seq (clist 1 2 3))) "(1 2 3)")
    (.assertTrue self (s.startswith? (repr (seq (range 3))) "( . seq(data=later"))
    (.assertTrue self (s.startswith? (repr (seq (cons 1 (range 3)))) "(1 . seq(data="))))

(export
  :objects [TestSeq])
