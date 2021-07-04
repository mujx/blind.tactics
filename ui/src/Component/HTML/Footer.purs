module Blind.Component.HTML.Footer where

import Halogen.HTML as Ht
import Halogen.HTML.Properties as Hp
import Blind.Component.HTML.Utils (css)

footer :: forall i p. Ht.HTML i p
footer =
  Ht.footer [ css "footer" ]
    [ Ht.div
        [ css "container" ]
        [ Ht.span_
            [ Ht.a
                [ css "clean-link text-muted"
                , Hp.target "_blank"
                , Hp.href "https://github.com/mujx/blind.tactics"
                ]
                [ Ht.text "blindfold chess training"
                ]
            ]
        ]
    ]
