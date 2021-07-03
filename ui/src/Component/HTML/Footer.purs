module Blind.Component.HTML.Footer where

import Halogen.HTML as Ht
import Blind.Component.HTML.Utils (css)

footer :: forall i p. Ht.HTML i p
footer =
  Ht.footer [ css "footer" ]
    [ Ht.div
        [ css "container" ]
        [ Ht.span [ css "text-muted" ] [ Ht.text "blindfold chess training" ] ]
    ]
