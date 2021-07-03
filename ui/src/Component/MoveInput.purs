module Blind.Component.MoveInput where

import Prelude
import Blind.Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as Ht
import Halogen.HTML.Elements as Elem
import Halogen.HTML.Events as He
import Halogen.HTML.Properties as Hp
import Blind.Data.FFI.Tagify as T

type State
  = { tagify :: Maybe T.Tagify
    , index :: Int
    , placeholderText :: String
    }

type Input
  = { index :: Int
    , placeholderText :: String
    }

initState :: Input -> State
initState input =
  { tagify: Nothing
  , index: input.index
  , placeholderText: input.placeholderText
  }

data Query a
  = GetTags (Array String -> a)
  | RemoveTags a

data Output
  = Add Int
  | Delete Int

data Action
  = Initialize
  | AddPathInput Int
  | DeletePathInput Int
  | Destroy

component :: forall m. MonadAff m => MonadEffect m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Initialize
              , finalize = Just Destroy
              , handleQuery = handleQuery
              , handleAction = handleAction
              }
    }
  where
  handleAction :: forall m1 s. MonadAff m1 => MonadEffect m1 => Action -> H.HalogenM State Action s Output m1 Unit
  handleAction Initialize = do
    state <- H.get
    tagify <- H.liftEffect $ T.mkTagify ("input#tag-entry-" <> show state.index)
    H.modify_ \s -> s { tagify = Just tagify }

  handleAction (AddPathInput idx) = H.raise $ Add idx

  handleAction (DeletePathInput idx) = H.raise $ Delete idx

  handleAction Destroy = do
    state <- H.get
    H.liftEffect $ T.destroy state.tagify

  handleQuery :: forall a m2. MonadEffect m2 => Query a -> H.HalogenM State Action () Output m2 (Maybe a)
  handleQuery (GetTags reply) = do
    state <- H.get
    tags <- H.liftEffect $ T.getTagsArray state.tagify
    pure $ Just $ reply tags

  handleQuery (RemoveTags a) = do
    state <- H.get
    H.liftEffect $ T.removeAllTags state.tagify
    pure $ Just a

  render state =
    Ht.div [ css "d-flex" ]
      [ Ht.div [ css "flex-grow-1" ]
          [ Elem.input
              [ css "move-input"
              , Hp.id ("tag-entry-" <> show state.index)
              , Hp.autofocus true
              , Hp.placeholder state.placeholderText
              ]
          ]
      , Ht.div [ css "d-flex mx-1" ]
          [ Ht.div
              [ css "p-1" ]
              [ Ht.button
                  [ css "btn btn-circle btn-outline-dark btn-sm"
                  , Hp.type_ Hp.ButtonButton
                  , He.onClick \_ -> AddPathInput state.index
                  ]
                  [ Ht.i [ css "fa fa-plus" ] [] ]
              ]
          , Ht.div [ css "py-1" ]
              [ Ht.button
                  [ css "btn btn-circle btn-outline-dark btn-sm"
                  , Hp.type_ Hp.ButtonButton
                  , He.onClick \_ -> DeletePathInput state.index
                  ]
                  [ Ht.i [ css "fa fa-minus" ] [] ]
              ]
          ]
      ]
