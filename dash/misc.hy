(require
  dash.core *)

(import
  dash.core *
  dash.strtools :as s)



(defn parse-args [spec [args None] #** parser-args]
  (import argparse)
  (let [parser (argparse.ArgumentParser #** parser-args)]
    (ap-for spec
            (let [#(args kwargs) (split-with (complement keyword?) it)
                  kwargs (->> (partition-all 2 kwargs)
                              (ap-map
                                (let [#(k v) it]
                                  #(k.name v)))
                              dict)]
              (.add-argument parser #* args #** kwargs)))
    (.parse-args parser args)))

(defmacro defmain [args #* body]
  `(when (= __name__ "__main__")
     (import sys)
     (sys.exit ((fn ~(or args '[#* _]) ~@body) #* sys.argv))))



(export
  :objects [parse-args]
  :macros [defmain do/a!])
