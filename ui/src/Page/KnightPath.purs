module Blind.Page.KnightPath where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Blind.Capability.Log (logDebug, logError, class Log)
import Blind.Capability.Now (class Now)
import Blind.Capability.Resource.KnightPathPuzzle (class ManageKnightPathPuzzle, getNextPuzzle)
import Blind.Component.HTML.Footer (footer)
import Data.Either (Either(..))
import Blind.Component.HTML.Header (header)
import Blind.Component.HTML.Utils (css)
import Blind.Component.Utils (setWindowTitle)
import Data.Maybe (Maybe(..))
import Blind.Data.Route (Route(..))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Blind.Component.KnightPath as KP
import Halogen as H
import Halogen.HTML as Ht

data Action
  = LoadNewProblem KP.Output
  | Initialize

type Slots
  = ( board :: H.Slot KP.Query KP.Output Int )

_board = Proxy :: Proxy "board"

type State
  = { result :: Boolean
    }

component ::
  forall q i o m.
  MonadAff m =>
  Now m =>
  Log m =>
  ManageKnightPathPuzzle m =>
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
      H.liftEffect $ setWindowTitle "Knight's Path - blind.tactics"
      loadProblem KP.GetInitialProblem
    LoadNewProblem o -> loadProblem o

  render _ =
    Ht.div_
      [ header KnightPath
      , Ht.div [ css "container-fluid" ]
          [ Ht.slot _board 0 KP.component "" LoadNewProblem
          ]
      , footer
      ]

loadProblem ::
  forall a o m.
  ManageKnightPathPuzzle m =>
  Log m =>
  Now m =>
  KP.Output -> H.HalogenM State a Slots o m Unit
loadProblem KP.GetInitialProblem = do
  logDebug "loading the initial knight path problem"
  res <- getNextPuzzle { maxSteps: 2 }
  case res of
    Left e -> logError e
    Right puzzle -> do
      _ <- H.tell _board 0 $ KP.RenderProblem { puzzle }
      pure unit

loadProblem (KP.GetNextProblem settings) = do
  logDebug "loading a new knight path problem"
  res <- getNextPuzzle { maxSteps: settings.maxSteps }
  case res of
    Left e -> logError e
    Right puzzle -> do
      _ <- H.tell _board 0 $ KP.RenderProblem { puzzle }
      pure unit
