(require
  dash.dash *)

(import
  dash.dash *
  dash.strtools :as s)



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



(defn a!name [a? name]
  (let [private? (s.starts-with? name "_")
        lower? (s.lower? name)]
    (s.concats
      (if private? "_" "")
      (if lower? (if a? "async-" "sync-") (if a? "Async" "Sync"))
      (if private? (cut name 1 None) name))))

(setv a!macros {'if/a! #((fn [async-form sync-form] async-form)
                          (fn [async-form sync-form] sync-form))
                'name/a! #((fn [sym] (symbol (a!name True (str sym))))
                            (fn [sym] (symbol (a!name False (str sym)))))
                'wait/a! #((fn [form] `(await ~form))
                            (fn [form] form))
                'next/a! #((fn [form] `(anext ~form))
                            (fn [form] `(next ~form)))
                'iter/a! #((fn [form] `(aiter ~form))
                            (fn [form] `(iter ~form)))
                'for/a! #((fn [bindings #* body] `(for [:async ~@bindings] ~@body))
                           (fn [bindings #* body] `(for [~@bindings] ~@body)))
                'with/a! #((fn [bindings #* body] `(with/a [~@bindings] ~@body))
                            (fn [bindings #* body] `(with [~@bindings] ~@body)))
                'fn/a! #((fn [#* body] `(fn/a ~@body))
                          (fn [#* body] `(fn ~@body)))
                'defn/a! #((fn [#* body] `(defn/a ~@body))
                            (fn [#* body] `(defn ~@body)))})

(defn a!replace [a? form]
  (cond (sexp? form) (let [#(car #* forms) form]
                       (if (and (symbol? car) (s.ends-with? car "/a!"))
                           (a!replace a? ((get a!macros car (if a? 0 1)) #* forms))
                           (sexp (-map (-partial a!replace a?) form))))
        (isinstance form hy.models.Tuple) (hy.models.Tuple (-map (-partial a!replace a?) form))
        (isinstance form hy.models.List)  (hy.models.List  (-map (-partial a!replace a?) form))
        (isinstance form hy.models.Set)   (hy.models.Set   (-map (-partial a!replace a?) form))
        (isinstance form hy.models.Dict)  (hy.models.Dict  (-map (-partial a!replace a?) form))
        True form))

(defmacro do/a! [#* body]
  `(do
     ~@(-map (-partial a!replace True) body)
     ~@(-map (-partial a!replace False) body)))



(export
  :objects [parse-args]
  :macros [defmain do/a!])
