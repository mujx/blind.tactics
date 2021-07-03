module Blind.Component.HTML.Games (component) where

import Halogen.HTML as Ht
import Halogen.HTML.Properties as Hp
import Blind.Component.HTML.Utils (css, safeHref)
import Blind.Data.Route (Route(..))

fancyCard :: forall i p r. { title :: String, desc :: String, link :: Route | r } -> Ht.HTML i p
fancyCard info =
  Ht.div [ css "card m-1 p-2 rounded-card frosted-glass" ]
    [ Ht.div [ css "card-body d-flex flex-column align-items-stretch" ]
        [ Ht.div [ css "game-title" ]
            [ Ht.text info.title
            ]
        , Ht.div [ css "game-desc" ]
            [ Ht.text info.desc
            ]
        ]
    , Ht.div [ css "d-flex justify-content-start m-3" ]
        [ Ht.div [ css "" ]
            [ Ht.button
                [ css "btn btn-light play-button", Hp.type_ Hp.ButtonButton
                ]
                [ Ht.span [ css "d-flex" ]
                    [ Ht.div_
                        [ Ht.a [ css "font-monospace play-button-text", safeHref info.link ]
                            [ Ht.text "PLAY"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

card :: forall i p. String -> String -> Ht.HTML i p
card name description =
  Ht.div [ css "card m-1 p-2 rounded-card frosted-glass" ]
    [ Ht.div [ css "card-body" ]
        [ Ht.h4 [ css "card-title m-3 font-weight-bold" ] [ Ht.text name ]
        , Ht.p [ css "card-body" ] [ Ht.text description ]
        ]
    ]

component :: forall i p. Ht.HTML i p
component =
  Ht.div [ css "container games-panel" ]
    [ Ht.div [ css "row header-container" ]
        [ Ht.div [ css "col" ]
            [ Ht.h1 [ css "home-title" ]
                [ Ht.text "blind.tactics"
                ]
            , Ht.h5_ [ Ht.text "Games and puzzles to train your board visualization" ]
            ]
        ]
    , Ht.div [ css "row mt-4" ]
        [ Ht.div [ css "col" ] [ fancyCard { title: "Find the checks", desc: "Given a starting position find all the ways to check the king without being captured.", link: FindChecks } ]
        , Ht.div [ css "col" ] [ fancyCard { title: "Blind tactics", desc: "Solve regular tactics problem by first following the game for a number of moves.", link: Problem "test" } ]
        , Ht.div [ css "col" ] [ fancyCard { title: "Knight's path", desc: "Enumerate all the paths that a knight could take to go from one sqaure to another.", link: KnightPath } ]
        , Ht.div [ css "col" ] [ fancyCard { title: "Common squares", desc: "Find all the squares that are currently attacked by all the pieces on the board.", link: CommonSquares } ]
        ]
    ]
