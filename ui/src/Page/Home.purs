module Blind.Page.Home where

import Prelude
import Halogen as H
import Halogen.HTML as Ht
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Blind.Data.Route (Route(..))
import Blind.Component.HTML.Games as Games
import Blind.Component.HTML.Header (header)
import Blind.Component.HTML.Footer (footer)
import Blind.Component.Utils (setWindowTitle)

data Action
  = Initialize

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  handleAction = case _ of
    Initialize -> H.liftEffect $ setWindowTitle "Home - blind.tactics"

  render _ =
    Ht.div_
      [ header Home
      , Games.component
      , footer
      ]
