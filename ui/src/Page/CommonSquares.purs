module Blind.Page.CommonSquares where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Blind.Capability.Log (logDebug, logError, logInfo, class Log)
import Blind.Capability.Now (class Now)
import Blind.Capability.Resource.CommonSquaresPuzzle (class ManageCommonSquaresPuzzle, getNextPuzzle)
import Blind.Component.HTML.Footer (footer)
import Data.Either (Either(..))
import Blind.Component.HTML.Header (header)
import Blind.Component.HTML.Utils (css)
import Blind.Component.Utils (setWindowTitle)
import Data.Maybe (Maybe(..))
import Blind.Data.Route (Route(..))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Blind.Component.CommonSquares as Cs
import Halogen as H
import Halogen.HTML as Ht

data Action
  = LoadNewProblem Cs.Output
  | Initialize

type Slots
  = ( board :: H.Slot Cs.Query Cs.Output Int )

_board = Proxy :: Proxy "board"

type State
  = { result :: Boolean
    }

component ::
  forall q i o m.
  MonadAff m =>
  Now m =>
  Log m =>
  ManageCommonSquaresPuzzle m =>
  MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const { result: true }
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
    Initialize -> do
      H.liftEffect $ setWindowTitle "Common Squares - blind.tactics"
      loadProblem Cs.GetInitialProblem
    LoadNewProblem o -> loadProblem o

  render _ =
    Ht.div_
      [ header (Problem "test")
      , Ht.div [ css "container-fluid" ] [ Ht.slot _board 0 Cs.component "" LoadNewProblem ]
      , footer
      ]

loadProblem ::
  forall a o m.
  ManageCommonSquaresPuzzle m =>
  Log m =>
  Now m =>
  Cs.Output -> H.HalogenM State a Slots o m Unit
loadProblem = case _ of
  Cs.GetInitialProblem -> do
    logDebug "loading initial common squares problem"
    res <- getNextPuzzle { maxPieces: 2 }
    case res of
      Left e -> logError e
      Right puzzle -> do
        logInfo $ show puzzle
        H.tell _board 0 $ Cs.RenderProblem { puzzle }
        pure unit
  (Cs.GetNextProblem settings) -> do
    logDebug "loading a new common squares problem"
    res <- getNextPuzzle settings
    case res of
      Left e -> logError e
      Right puzzle -> do
        logInfo $ show puzzle
        H.tell _board 0 $ Cs.RenderProblem { puzzle }
        pure unit
