module Blind.Page.FindChecks where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Blind.Capability.Log
  ( logDebug
  , logError
  , logInfo
  , class Log
  )
import Blind.Capability.Now (class Now)
import Blind.Capability.Resource.ChecksPuzzle (class ManageChecksPuzzle, getNextPuzzle)
import Blind.Component.HTML.Footer (footer)
import Data.Either (Either(..))
import Blind.Component.HTML.Header (header)
import Blind.Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Blind.Data.Route (Route(..))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Blind.Component.FindChecks as FC
import Blind.Component.Utils (setWindowTitle)
import Halogen as H
import Halogen.HTML as Ht

data Action
  = LoadNewProblem FC.Output
  | Initialize

type Slots
  = ( board :: H.Slot FC.Query FC.Output Int )

_board = Proxy :: Proxy "board"

type State
  = { result :: Boolean
    }

component ::
  forall q i o m.
  MonadAff m =>
  Now m =>
  Log m =>
  ManageChecksPuzzle m =>
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
      H.liftEffect $ setWindowTitle "Find The Checks - blind.tactics"
      loadProblem FC.GetInitialProblem
    LoadNewProblem o -> loadProblem o

  render _ =
    Ht.div_
      [ header (Problem "test")
      , Ht.div [ css "container-fluid" ]
          [ Ht.slot _board 0 FC.component "" LoadNewProblem
          ]
      , footer
      ]

loadProblem ::
  forall a o m.
  ManageChecksPuzzle m =>
  Log m =>
  Now m =>
  FC.Output -> H.HalogenM State a Slots o m Unit
loadProblem FC.GetInitialProblem = do
  logDebug "loading a new checks problem"
  res <-
    getNextPuzzle
      { nbWhite: (FC.defaultSettings).nbWhite
      , nbBlack: (FC.defaultSettings).nbBlack
      }
  case res of
    Left e -> do
      logError e
      -- TODO: Show error on the UI
      pure unit
    Right puzzle -> do
      logInfo $ show puzzle
      _ <- H.tell _board 0 $ FC.RenderProblem { puzzle }
      pure unit

loadProblem (FC.GetNextProblem settings) = do
  logDebug "loading a new checks problem"
  res <- getNextPuzzle settings
  case res of
    Left e -> do
      logError e
      -- TODO: Show error on the UI
      pure unit
    Right puzzle -> do
      logInfo $ show puzzle
      _ <- H.tell _board 0 $ FC.RenderProblem { puzzle }
      pure unit
