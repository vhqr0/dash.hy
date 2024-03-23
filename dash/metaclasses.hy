(require
  dash.core.polyfill *)

(import
  dash.core.polyfill *)

(defclass cast-meta [type]
  "For class C, object o: C(o) cast o to C.
Avoid creating new object if o is an instance of C, return o itself."

  (defn __call__ [self [o None]]
    (if (isinstance o self) o (.__call__ (super) o))))

(defclass singleton-meta [type]
  "For class C: cache C() to C.instance.
Avoid creating new object if there is a cached instance."

  (defn __init__ [self #* args #** kwargs]
    (.__init__ (super) #* args #** kwargs)
    (setv self.instance None))

  (defn __call__ [self]
    (let [instance self.instance]
      (->> instance
           (do
             (when (none? instance)
               (setv instance (.__call__ (super))
                     self.instance instance)))))))

(export
  :objects [cast-meta])
