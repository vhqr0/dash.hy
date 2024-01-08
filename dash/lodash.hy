(require
  dash.dash *)

(import
  dash.dash *
  dash.dash
  dash.operator :as o
  dash.strtools :as s)

(setv __all__ [])

(defn lodash-name-add-prefix [lodash-name prefix]
  (cond (s.starts-with? lodash-name "__") (s.concats "__" prefix (cut lodash-name 2 None))
        (s.starts-with? lodash-name "_")  (s.concats "_" prefix (cut lodash-name 1 None))
        True (s.concats prefix lodash-name)))

(defn lodash-name-from-name [name]
  (let [dash-name (hy.unmangle name)
        lodash-name (s.replace dash-name "-" "_")]
    (cond (s.ends-with? lodash-name "?") (lodash-name-add-prefix (cut lodash-name -1) "is_")
          (s.ends-with? lodash-name "!") (lodash-name-add-prefix (cut lodash-name -1) "do_")
          True lodash-name)))

(defn load-name [prefix module name]
  (let [lodash-name (s.concats prefix (lodash-name-from-name name))]
    (setv (get (globals) lodash-name) (getattr module name))
    (.append __all__ lodash-name)))

(defn load-all-name [prefix module]
  (--each module.__all__ (load-name prefix module it)))

(load-all-name "" dash.dash)
(load-all-name "s_" s)
(load-all-name "o_" o)
