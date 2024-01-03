(setv add        hy.pyops.+
      mul        hy.pyops.*
      matmul     hy.pyops.@
      pow        hy.pyops.**
      sub        hy.pyops.-
      truediv    hy.pyops./
      div        hy.pyops.//
      mod        hy.pyops.%
      bit-not    hy.pyops.bnot
      bit-and    hy.pyops.&
      bit-or     hy.pyops.|
      bit-xor    hy.pyops.^
      bit-lshift hy.pyops.<<
      bit-rshift hy.pyops.>>
      eq?        hy.pyops.=
      not-eq?    hy.pyops.!=
      lt?        hy.pyops.<
      le?        hy.pyops.<=
      gt?        hy.pyops.>
      ge?        hy.pyops.>=
      not?       hy.pyops.not
      and?       hy.pyops.and
      or?        hy.pyops.or
      is?        hy.pyops.is
      in?        hy.pyops.in
      not-is?    hy.pyops.is-not
      not-in?    hy.pyops.not-in)

(export
  :objects [add mul matmul pow sub truediv div mod
            bit-not bit-and bit-or bit-xor bit-lshift bit-rshift
            eq? not-eq? lt? le? gt? ge? not? and? or? is? in? not-is? not-in?])
