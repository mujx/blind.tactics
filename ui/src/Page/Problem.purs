module Blind.Page.Problem where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Blind.Capability.Log (logDebug, logError, class Log)
import Blind.Capability.Now (class Now)
import Blind.Capability.Resource.TacticsPuzzle (class ManageTacticsPuzzle, getNextPuzzle)
import Blind.Component.ChessGround (Query(..))
import Blind.Component.ChessGround as Cg
import Blind.Component.HTML.Footer (footer)
import Blind.Component.HTML.Header (header)
import Blind.Component.HTML.Utils (css)
import Blind.Data.ProblemInput (Orientation(..), ProblemInput)
import Blind.Data.Route (Route(..))
import Blind.Data.Tactics.Puzzle (mkProblemInput)
import Blind.Component.Utils (setWindowTitle)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as Ht

data Action
  = LoadNewProblem Cg.Output
  | Initialize

type Slots
  = ( board :: H.Slot Cg.Query Cg.Output Int )

_board = Proxy :: Proxy "board"

type State
  = { problem :: ProblemInput
    , result :: Boolean
    }

startingPosition :: ProblemInput
startingPosition =
  { fen: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  , moves: []
  , orientation: White
  , solution: { "orig": "e2", "dest": Just "e4", brush: "green" }
  }

component ::
  forall q i o m.
  MonadAff m =>
  MonadEffect m =>
  ManageTacticsPuzzle m =>
  Now m => Log m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        const
          { problem: startingPosition
          , result: false
          }
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
      H.liftEffect $ setWindowTitle "Tactics - blind.tactics"
      loadProblem Cg.GetInitialProblem
    LoadNewProblem opts -> loadProblem opts

  render state =
    Ht.div_
      [ header (Problem "test")
      , Ht.div [ css "container-fluid" ]
          [ Ht.slot _board 0 Cg.component state.problem LoadNewProblem
          ]
      , footer
      ]

loadProblem ::
  forall a o m.
  ManageTacticsPuzzle m =>
  Log m =>
  Now m =>
  Cg.Output -> H.HalogenM State a Slots o m Unit
loadProblem Cg.GetInitialProblem = _fetchProblem 1500 5

loadProblem (Cg.CorrectSolution { rating, movesToFollow }) = _fetchProblem rating movesToFollow

loadProblem Cg.WrongSolution = _fetchProblem 1500 5

_fetchProblem ::
  forall a o m.
  ManageTacticsPuzzle m =>
  Log m =>
  Now m => Int -> Int -> H.HalogenM State a Slots o m Unit
_fetchProblem rating movesToFollow = do
  logDebug "loading a new problem"
  res <- getNextPuzzle rating movesToFollow
  case res of
    Left e -> do
      logError e
      -- TODO: Show error on the UI
      pure unit
    Right puzzle -> do
      let
        newProblem = mkProblemInput puzzle
      H.modify_ \s -> s { problem = newProblem }
      _ <- H.tell _board 0 $ RenderProblem { problem: newProblem, puzzle: puzzle }
      pure unit
