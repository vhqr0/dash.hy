(require
  dash *)

(import
  dash *
  dash.strtools :as s
  dataclasses [dataclass])



(defclass BufferError [RuntimeError])
(defclass BufferWantReadError [BufferError])
(defclass BufferWantWriteError [BufferError])

(defclass [(dataclass :slots True)] BufferReader []
  #^ Buffer data
  (setv #^ int pos 0)

  (defn [property] view [self]
    (cut (memoryview self.data) self.pos None))

  (defn readall [self]
    #((BufferReader self.data (len self.data)) (bytes self.view)))

  (defn readexactly [self n]
    (let [view self.view]
      (if (chainc 0 <= n <= (len view))
          #((BufferReader self.data (+ self.pos n)) (bytes (cut view n)))
          (raise BufferWantReadError))))

  (defn readuntil [self [sep b"\n"] [keepend False]]
    (let [start self.pos
          end1 (try
                 (s.index self.data start)
                 (except [e ValueError]
                   (raise BufferWantReadError :from e)))
          end2 (+ end1 (len sep))]
      #((BufferReader self.data end2)
         (bytes (cut self.view (if keepend end2 end1))))))

  (defn readeof [self]
    (if self.view (raise BufferWantWriteError) self)))



(export
  :objects [BufferWantReadError BufferWantWriteError BufferReader])
