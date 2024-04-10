;; s: str
;; b: bytes
;; x: str or bytes

(defn encode [s [encoding "utf-8"] [errors "strict"]] (.encode s encoding errors))
(defn decode [b [encoding "utf-8"] [errors "strict"]] (.decode b encoding errors))

(defn format [x #* args #** kwargs] (.format f #* args #** kwargs))
(defn format-map [x mapping] (.format-map f mapping))

(defn empty? [x] (zero? (len x)))



(defn join-in [sep xs] (.join sep xs))
(defn concats-in [ss] (join-in "" ss))
(defn concatb-in [bs] (join-in b"" bs))

(defn join [sep #* xs] (join-in sep xs))
(defn concats [#* ss] (concats-in ss))
(defn concatb [#* bs] (concatb-in bs))

(defn splitlines [x [keepends False]] (.splitlines x :keepends keepends))

(defn split  [x [sep None] [maxsplit -1]] (.split  x sep maxsplit))
(defn rsplit [x [sep None] [maxsplit -1]] (.rsplit x sep maxsplit))

(defn partition  [x sep] (.partition  x sep))
(defn rpartition [x sep] (.rpartition x sep))



(defn includes? [x sub] (in sub x))

;; args: [start [end]]
(defn startswith? [x prefix #* args] (.startswith x prefix #* args))
(defn endswith?   [x suffix #* args] (.endswith   x suffix #* args))

;; args: [start [end]]
(defn count  [x sub #* args] (.count  x sub #* args))
(defn index  [x sub #* args] (.index  x sub #* args))
(defn rindex [x sub #* args] (.rindex x sub #* args))
(defn find   [x sub #* args] (.find   x sub #* args))
(defn rfind  [x sub #* args] (.rfind  x sub #* args))

(defn replace [x old new [count -1]] (.replace x old new count))

(defn strip  [x [chars None]] (.strip  x chars))
(defn lstrip [x [chars None]] (.lstrip x chars))
(defn rstrip [x [chars None]] (.rstrip x chars))

(defn removeprefix [x prefix [strict False]]
  (when (and strict (not (startswith? prefix)))
    (raise ValueError))
  (.removeprefix x prefix))

(defn removesuffix [x suffix [strict False]]
  (when (and strict (not (endswith? suffix)))
    (raise ValueError))
  (.removesuffix x suffix))



(defn ascii?     [x] (.isascii x))
(defn space?     [x] (.isspace x))
(defn alpha?     [x] (.isalpha x))
(defn lower?     [x] (.islower x))
(defn upper?     [x] (.isupper x))
(defn title?     [x] (.istitle x))
(defn digit?     [x] (.isdigit x))
(defn decimal?   [x] (.isdecimal x))
(defn numeric?   [x] (.isnumeric x))
(defn alnum?     [x] (.isalnum x))
(defn printable? [x] (.isprintable x))

(defn capitalize [x] (.capitalize x))
(defn lower      [x] (.lower x))
(defn upper      [x] (.upper x))
(defn title      [x] (.title x))
(defn swapcase   [x] (.swapcase x))
(defn casefold   [x] (.casefold x))

;; str: [fillchar " "]
;; bytes: [fillchar b" "]
(defn center [x width #* args] (.center x width #* args))
(defn ljust  [x width #* args] (.ljust  x width #* args))
(defn rjust  [x width #* args] (.rjust  x width #* args))

(defn zfill  [x width] (.zfill x width))

(defn expandtabs [x [tabsize 8]] (.expandtabs x tabsize))



(export
  :objects [encode decode format format-map empty?
            join-in concats-in concatb-in join concats concatb
            splitlines split rsplit partition rpartition
            includes? startswith? endswith? count index rindex find rfind
            replace strip lstrip rstrip removeprefix removesuffix
            ascii? space? alpha? lower? upper? title?
            digit? decimal? numeric? alnum? printable?
            capitalize lower upper title swapcase casefold
            center ljust rjust zfill expandtabs])
