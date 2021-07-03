module Blind.Component.ChessBoard (component) where

import Prelude
import Blind.Capability.Log (class Log)
import Blind.Capability.Now (class Now)
import Blind.Component.FFI.ChessGround as Cg
import Blind.Component.HTML.Utils (css)
import Halogen.HTML.Properties as Hp
import Halogen.HTML as Ht
import Data.Maybe (Maybe(..))
import Halogen as H
import Effect.Class (class MonadEffect)
import Blind.Data.Shape (Shape)
import Data.Set as Set

type BoardInput
  = { fen :: String
    , shapes :: Set.Set Shape
    }

data BoardAction
  = InitializeBoard
  | DestroyBoard

component ::
  forall q o m.
  Now m =>
  Log m => MonadEffect m => H.Component q BoardInput o m
component =
  H.mkComponent
    { initialState: initialBoardState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleBoardAction
              , initialize = Just InitializeBoard
              , finalize = Just DestroyBoard
              }
    }
  where
  boardEl :: String
  boardEl = "chess-ground-section"

  initialBoardState input =
    { board: Nothing
    , fen: input.fen
    , cfg: Cg.initConfig { fen = input.fen }
    , shapes: input.shapes
    }

  handleBoardAction InitializeBoard = do
    state <- H.get
    cg <- H.liftEffect $ Cg.mkChessGround boardEl state.cfg
    H.liftEffect Cg.addResizeListener
    H.liftEffect $ Cg.setShapes (Just cg) (Set.toUnfoldable state.shapes)
    H.modify_ \s -> s { board = Just cg }

  handleBoardAction DestroyBoard = do
    H.liftEffect Cg.removeResizeListener
    state <- H.get
    H.liftEffect $ Cg.destroy state.board
    H.modify_ \s -> s { board = Nothing }

  render _ = Ht.div [ css "blue cburnett" ] [ Ht.div [ Hp.id boardEl ] [] ]
