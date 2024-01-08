;; s: str
;; b: bytes
;; o: str or bytes

(defn encode [s [encoding "utf-8"] [errors "strict"]] (.encode s encoding errors))
(defn decode [b [encoding "utf-8"] [errors "strict"]] (.decode b encoding errors))

(defn format [f #* args #** kwargs] (.format f #* args #** kwargs))
(defn format-map [f mapping] (.format-map f mapping))

(defn join-in [sep os] (.join sep os))
(defn join [sep #* os] (join-in sep os))

(defn concats-in [ss] (join-in "" ss))
(defn concats [#* ss] (concats-in ss))
(defn concatb-in [bs] (join-in b"" bs))
(defn concatb [#* bs] (concatb-in bs))

(defn split-lines [o] (.splitlines o))

(defn split  [o [sep None] [maxsplit -1]] (.split  o sep maxsplit))
(defn rsplit [o [sep None] [maxsplit -1]] (.rsplit o sep maxsplit))

(defn partition  [o sep] (.partition  o sep))
(defn rpartition [o sep] (.rpartition o sep))



(defn subs [o start [end None]] (cut o start end))

(defn includes? [o sub] (in sub o))

;; args: [count]
(defn replace [o old new #* args] (.replace o old new #* args))

;; args: [start [end]]
(defn count  [o sub #* args] (.count  o sub #* args))
(defn index  [o sub #* args] (.index  o sub #* args))
(defn rindex [o sub #* args] (.rindex o sub #* args))
(defn find   [o sub #* args] (.find   o sub #* args))
(defn rfind  [o sub #* args] (.rfind  o sub #* args))



(defn strip  [o [chars None]] (.strip  o chars))
(defn lstrip [o [chars None]] (.lstrip o chars))
(defn rstrip [o [chars None]] (.rstrip o chars))

(defn remove-prefix [o prefix] (.removeprefix o prefix))
(defn remove-suffix [o suffix] (.removesuffix o suffix))

;; args: [start [end]]
(defn starts-with? [o prefix #* args] (.startswith o prefix #* args))
(defn ends-with?   [o suffix #* args] (.endswith   o suffix #* args))



(defn blank?     [o] (not o))
(defn ascii?     [o] (.isascii o))
(defn space?     [o] (.isspace o))
(defn alpha?     [o] (.isalpha o))
(defn lower?     [o] (.islower o))
(defn upper?     [o] (.isupper o))
(defn title?     [o] (.istitle o))
(defn digit?     [o] (.isdigit o))
(defn decimal?   [o] (.isdecimal o))
(defn numeric?   [o] (.isnumeric o))
(defn alnum?     [o] (.isalnum o))
(defn printable? [o] (.isprintable o))

(defn capitalize [o] (.capitalize o))
(defn lower-case [o] (.lower o))
(defn upper-case [o] (.upper o))
(defn title-case [o] (.title o))
(defn swap-case  [o] (.swapcase o))
(defn case-fold  [o] (.casefold o))



;; str: [fillchar " "]
;; bytes: [fillchar b" "]
(defn center [o width #* args] (.center o width #* args))
(defn ljust  [o width #* args] (.ljust  o width #* args))
(defn rjust  [o width #* args] (.rjust  o width #* args))

(defn zfill  [o width] (.zfill o width))

(defn expandtabs [o [tabsize 8]] (.expandtabs o tabsize))

(export
  :objects [encode decode format format-map
            join-in join concats-in concats concatb-in concatb
            split rsplit split-lines partition rpartition
            subs includes? replace count index rindex find rfind
            strip lstrip rstrip remove-prefix remove-suffix starts-with? ends-with?
            blank? ascii? space? alpha? lower? upper? title?
            digit? decimal? numeric? alnum? printable?
            capitalize lower-case upper-case title-case swap-case case-fold
            center ljust rjust zfill expandtabs])
