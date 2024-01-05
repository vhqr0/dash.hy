(require
  dash.dash *)

(import
  dash.dash *)

(defn parse-args [spec [args None] #** parser-args]
  (import argparse)
  (let [parser (argparse.ArgumentParser #** parser-args)]
    (--each spec
            (let [#(args kwargs) (-split-with (-notfn keyword?) it)
                  kwargs (dict (-map-unzipped (fn [k v] #(k.name v)) (-partition-all 2 kwargs)))]
              (.add-argument parser #* args #** kwargs)))
    (.parse-args parser args)))

(defmacro defmain [args #* body]
  `(when (= __name__ "__main__")
     (import sys)
     (sys.exit ((fn ~(or args '[#* _]) ~@body) #* sys.argv))))

(export
  :objects [parse-args]
  :macros [defmain])
