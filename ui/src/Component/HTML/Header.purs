-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Blind.Component.HTML.Header where

import Prelude
import Blind.Component.HTML.Utils (css, safeHref)
import Blind.Data.Route (Route(..))
import Data.Monoid (guard)
import Halogen.HTML as Ht
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties as Hp
import Halogen.HTML.Properties.ARIA as Aria

-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not.
header :: forall i p. Route -> Ht.HTML i p
header route =
  Ht.nav
    [ css "navbar navbar-expand-lg navbar-dark bg-dark" ]
    [ Ht.div
        [ css "container d-flex" ]
        [ Ht.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ Ht.text "blind.tactics" ]
        , Ht.button
            [ css "navbar-toggler"
            , Hp.type_ Hp.ButtonButton
            , attr "data-toggle" "collapse"
            , attr "data-target" "#navbarNav"
            , Aria.controls "navbarNav"
            , Aria.expanded "false"
            , Aria.label "Toggle navigation"
            ]
            [ Ht.span [ css "navbar-toggler-icon" ] []
            ]
        , Ht.div
            [ css "collapse navbar-collapse", Hp.id "navbarNav" ]
            [ Ht.ul
                [ css "navbar-nav" ]
                [ navItem Home [ Ht.text "Home" ]
                , navItem (Problem "test") [ Ht.text "Tactics" ]
                , Ht.li [ css "nav-item dropdown" ]
                    [ Ht.a
                        [ css "nav-link dropdown-toggle"
                        , Hp.href "#"
                        , Hp.id "navbarDropdownMenuLink"
                        , Hp.attr (Ht.AttrName "role") "button"
                        , Hp.attr (Ht.AttrName "data-bs-toggle") "dropdown"
                        , Hp.attr (Ht.AttrName "aria-expanded") "false"
                        ]
                        [ Ht.text "Games" ]
                    , Ht.ul [ css "dropdown-menu", Hp.attr (Ht.AttrName "aria-labelledby") "navbarDropdownMenuLink" ]
                        [ Ht.li_ [ Ht.a [ css "dropdown-item", safeHref FindChecks ] [ Ht.text "Find the checks" ] ]
                        , Ht.li_ [ Ht.a [ css "dropdown-item", safeHref KnightPath ] [ Ht.text "Knight's path" ] ]
                        , Ht.li_ [ Ht.a [ css "dropdown-item", safeHref CommonSquares ] [ Ht.text "Common squares" ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]
  where
  attr key value = Hp.attr (AttrName key) value

  navItem r html =
    Ht.li
      [ css $ "nav-item" <> guard (route == r) " active" ]
      [ Ht.a
          [ css "nav-link"
          , safeHref r
          ]
          html
      ]
