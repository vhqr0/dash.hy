(require
  dash.dash.polyfill *)

(import
  dash.dash.polyfill *)


;;; cast-meta
;; for class C, object o: C(o) cast object o to C, if o is instance of
;; C, avoid creating new object, return o itself.

(defn cast-meta-call [self [o None]]
  (if (isinstance o self) o (.__call__ (super cast-meta self) o)))

(setv cast-meta (type "cast-meta" #(type) {"__call__" cast-meta-call}))

(export
  :objects [cast-meta])
