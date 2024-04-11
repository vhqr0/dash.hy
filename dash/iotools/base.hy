(require
  dash *)

(eval-and-compile
  (import
    dash *
    dash.strtools :as s))


;; do/a!

(eval-and-compile
  (let [pattern (hy.I.re.compile "^(_*)(.*)$")]
    (defn a!name [a? name]
      (let [#(prefix name) (.groups (.match pattern name))]
        (s.concats
          prefix
          (if (s.lower? name)
              (if a? "async-" "sync-")
              (if a? "Async" "Sync"))
          name))))

  (setv a!macros {'if/a! #((fn [async-form sync-form] async-form)
                            (fn [async-form sync-form] sync-form))
                  'name/a! #((fn [sym] (symbol (a!name True sym)))
                              (fn [sym] (symbol (a!name False sym))))
                  'wait/a! #((fn [form] `(await ~form))
                              (fn [form] form))
                  'for/a! #((fn [bindings #* body] `(for [:async ~@bindings] ~@body))
                             (fn [bindings #* body] `(for [~@bindings] ~@body)))
                  'with/a! #((fn [bindings #* body] `(with/a [~@bindings] ~@body))
                              (fn [bindings #* body] `(with [~@bindings] ~@body)))
                  'loop/a! #((fn [bindings form] `(loop [~@bindings] ~form))
                              (fn [bindings form] `(loop :async [~@bindings] ~form)))
                  'fn/a! #((fn [#* body] `(fn/a ~@body))
                            (fn [#* body] `(fn ~@body)))
                  'defn/a! #((fn [#* body] `(defn/a ~@body))
                              (fn [#* body] `(defn ~@body)))})

  (defn a!expand [a? form]
    (cond (sexp? form) (let [#(x #* xs) form]
                         (if (and (symbol? x) (s.endswith? x "/a!"))
                             (a!expand a? ((get a!macros x (if a? 0 1)) #* xs))
                             (sexp (map (partial a!expand a?) form))))
          (hycoll? form) (form.__class__ (map (partial a!expand a?) form))
          True form)))

(defmacro do/a! [#* body]
  `(do
     ~@(map (partial a!expand False) body)
     ~@(map (partial a!expand True) body)))

(defmacro export/a! [#* syms]
  `(.extend __all__
            [~@(->> syms
                    (ap-mapcat
                      #((a!name False it) (a!name True it)))
                    (map hy.mangle))]))


;; async aliases

(import time threading queue asyncio)

(do/a!
  (setv (name/a! next)               (if/a! anext next)
        (name/a! iter)               (if/a! aiter iter)
        (name/a! StopIteration)      (if/a! StopAsyncIteration StopIteration)
        (name/a! Iterator)           (if/a! Iterator AsyncIterator)
        (name/a! Generator)          (if/a! Generator AsyncGenerator)
        (name/a! Iterable)           (if/a! Iterable AsyncIterable)
        (name/a! iter?)              (if/a! aiter? iter?)
        (name/a! generator?)         (if/a! generator? agenerator?)
        (name/a! iterable?)          (if/a! iterable? aiterable?)
        (name/a! sleep)              (. (if/a! asyncio time)      sleep)
        (name/a! Lock)               (. (if/a! asyncio threading) Lock)
        (name/a! Event)              (. (if/a! asyncio threading) Event)
        (name/a! Condition)          (. (if/a! asyncio threading) Condition)
        (name/a! Semaphore)          (. (if/a! asyncio threading) Semaphore)
        (name/a! BoundedSemaphore)   (. (if/a! asyncio threading) BoundedSemaphore)
        (name/a! Barrier)            (. (if/a! asyncio threading) Barrier)
        (name/a! BrokenBarrierError) (. (if/a! asyncio threading) BrokenBarrierError)
        (name/a! Queue)              (. (if/a! asyncio queue)     Queue)
        (name/a! PriorityQueue)      (. (if/a! asyncio queue)     PriorityQueue)
        (name/a! LifoQueue)          (. (if/a! asyncio queue)     LifoQueue)
        (name/a! QueueEmpty)         (. (if/a! asyncio queue)     (if/a! QueueEmpty Empty))
        (name/a! QueueFull)          (. (if/a! asyncio queue)     (if/a! QueueFull Full))))



(export
  :objects []
  :macros [do/a! export/a!])

(export/a!
  next iter StopIteration Iterator Generator Iterable iter? generator? iterable?
  sleep Lock Event Condition Semaphore BoundedSemaphore Barrier BrokenBarrierError
  Queue PriorityQueue LifoQueue QueueEmpty QueueFull)
