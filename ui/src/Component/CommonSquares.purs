module Blind.Component.CommonSquares where

import Prelude
import Blind.Capability.Log (class Log)
import Blind.Capability.Now (class Now)
import Blind.Component.FFI.ChessGround as Cg
import Blind.Component.HTML.Utils (css, whenElem)
import Blind.Data.Tactics.Puzzle (CommonSquaresPuzzle)
import Blind.Component.Utils (moveListCard, runDelayedAction)
import Data.Int (fromString)
import Data.Set as Set
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as Ht
import Halogen.HTML.Events as He
import Halogen.HTML.Elements as Elem
import Halogen.HTML.Properties as Hp
import Blind.Component.MoveInput as MoveInput
import Blind.Component.ChessBoard as ChessBoard
import Blind.Data.FFI.Tagify as T
import Blind.Data.Shape (Shape)

type Slots
  = ( moveInput :: H.Slot MoveInput.Query MoveInput.Output Int
    , chessboard :: forall query. H.Slot query Void Int
    )

type GameSettings
  = { maxPieces :: Int
    , showBoard :: Boolean
    , moveToNext :: Boolean
    }

defaultSettings :: GameSettings
defaultSettings =
  { maxPieces: 2
  , showBoard: false
  , moveToNext: false
  }

data Action
  = Initialize
  | SettingsChanged GameSettings
  | SubmitSolution
  | HandleMoveInputEvent MoveInput.Output
  | NextProblem
  | Destroy

data Output
  = GetNextProblem GameSettings
  | GetInitialProblem

data Query a
  = RenderProblem { puzzle :: CommonSquaresPuzzle } a

type State
  = { submitted :: Boolean
    , board :: Maybe Cg.ChessBoard
    , tagify :: Maybe T.Tagify
    , puzzle :: Maybe CommonSquaresPuzzle
    , settings :: GameSettings
    , correctMoves :: Set.Set String
    , movesGiven :: Set.Set String
    , wrongMoves :: Set.Set String
    , missedMoves :: Set.Set String
    }

initState :: State
initState =
  { submitted: false
  , board: Nothing
  , puzzle: Nothing
  , tagify: Nothing
  , settings: defaultSettings
  , correctMoves: Set.empty
  , movesGiven: Set.empty
  , wrongMoves: Set.empty
  , missedMoves: Set.empty
  }

_moveInput = Proxy :: Proxy "moveInput"

displayPieces :: Array String -> String
displayPieces pieces = intercalate " " pieces

positionDisplay :: forall i p. State -> Ht.HTML i p
positionDisplay state =
  Ht.div_
    [ Ht.h1 [ css "text-center text-capitalize" ] [ Ht.text "Position:" ]
    , Ht.h3 [ css "text-center font-monospace" ] [ Ht.text $ displayPieces (maybe [] (\p -> p.pieces) state.puzzle) ]
    ]

puzzleControls :: forall m. State -> Ht.ComponentHTML Action Slots m
puzzleControls state =
  Ht.div_
    [ Ht.label
        [ Hp.attr (Ht.AttrName "for") "max-pieces", css "form-label"
        ]
        [ Ht.text $ "Max pieces: " <> (show state.settings.maxPieces) ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1.0
        , Hp.max 5.0
        , Hp.value (show state.settings.maxPieces)
        , Hp.step (Hp.Step 1.0)
        , Hp.type_ Hp.InputRange
        , Hp.id "max-pieces"
        , He.onValueChange \v ->
            ( SettingsChanged
                ( state.settings
                    { maxPieces = fromMaybe defaultSettings.maxPieces $ fromString v
                    }
                )
            )
        ]
    , Ht.div [ css "form-check form-switch" ]
        [ Ht.input
            [ css "form-check-input"
            , Hp.type_ Hp.InputCheckbox
            , Hp.id "show-board-switch"
            , Hp.value (show state.settings.showBoard)
            , He.onChecked \v -> (SettingsChanged (state.settings { showBoard = v }))
            ]
        , Ht.label [ css "form-check-label", Hp.for "show-board-switch" ] [ Ht.text "Show the board" ]
        ]
    , Ht.div [ css "form-check form-switch" ]
        [ Ht.input
            [ css "form-check-input"
            , Hp.type_ Hp.InputCheckbox
            , Hp.id "show-next-switch"
            , Hp.value (show state.settings.moveToNext)
            , He.onChecked \v ->
                (SettingsChanged (state.settings { moveToNext = v }))
            ]
        , Ht.label [ css "form-check-label", Hp.for "show-next-switch" ] [ Ht.text "Move to next immediately" ]
        ]
    ]

resultField :: forall i p. State -> Ht.HTML i p
resultField state = do
  if isCorrect state then
    Ht.div [ css "alert alert-success" ]
      [ Ht.span_ [ Ht.i [ css "fa fa-check", Hp.style "margin-right: 8px" ] [], Ht.text "Correct!" ]
      ]
  else
    Ht.div_
      [ Ht.div [ css "alert alert-danger" ]
          [ Ht.span_ [ Ht.i [ css "fa fa-times", Hp.style "margin-right: 8px" ] [], Ht.text "Wrong solution!" ]
          ]
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> moveListCard state.wrongMoves "Wrong squares")
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> Ht.div [ css "mt-3" ] [])
      , whenElem (state.missedMoves /= Set.empty) (\_ -> moveListCard state.missedMoves "Missed squares")
      ]

computeShapes :: State -> Set.Set Shape
computeShapes state =
  ( (Set.map (\x -> { orig: x, dest: Nothing, brush: "red" }) state.missedMoves)
      <> (Set.map (\x -> { orig: x, dest: Nothing, brush: "green" }) state.correctMoves)
  )

component :: forall m i. MonadAff m => Now m => Log m => MonadEffect m => H.Component Query i Output m
component =
  H.mkComponent
    { initialState: const initState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , handleQuery = handleQuery
              , finalize = Just Destroy
              }
    }
  where
  render state =
    Ht.div [ css "d-flex find-the-checks-wrapper" ]
      [ Ht.aside [ css "col-3 d-flex flex-column px-4 left-panel" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Instructions" ]
              , Ht.div [ css "card-body" ]
                  [ Ht.text "Find the squares that are attacked by all pieces."
                  ]
              ]
          , Ht.div [ css "mt-4" ] []
          , Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Settings" ]
              , Ht.div [ css "card-body" ] [ puzzleControls state ]
              ]
          ]
      , Ht.div [ css "col-6 d-flex justify-content-center flex-column" ]
          [ if state.submitted && state.settings.showBoard then
              Ht.slot (Proxy :: _ "chessboard") 0 ChessBoard.component
                { fen: maybe "" (\puzzle -> puzzle.fen) state.puzzle
                , shapes: computeShapes state
                }
                absurd
            else
              positionDisplay state
          ]
      , Ht.aside [ css "col-3 d-flex flex-column flex-fill px-4" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Solution" ]
              , Ht.div [ css "card-body" ]
                  [ Ht.div [ css "row" ]
                      [ Ht.div [ css "col-lg-12" ]
                          [ Elem.input
                              [ css "move-input"
                              , Hp.autofocus true
                              , Hp.placeholder "e.g f3, g5"
                              ]
                          ]
                      ]
                  , Ht.div [ css "mt-3" ] []
                  , Ht.div [ css "d-flex justify-content-end" ]
                      [ whenElem (not state.submitted)
                          ( \_ ->
                              Ht.button
                                [ css "btn btn-dark"
                                , Hp.type_ Hp.ButtonButton
                                , He.onClick \_ -> SubmitSolution
                                ]
                                [ Ht.text "SUBMIT" ]
                          )
                      , whenElem (state.submitted)
                          ( \_ ->
                              Ht.button
                                [ css "btn btn-primary"
                                , Hp.type_ Hp.ButtonButton
                                , He.onClick \_ -> NextProblem
                                ]
                                [ Ht.text "NEXT" ]
                          )
                      ]
                  ]
              ]
          , Ht.div [ css "mt-4" ] []
          , whenElem (state.submitted) (const $ resultField state)
          ]
      , Ht.div [ css "mt-4" ] []
      ]

isCorrect :: State -> Boolean
isCorrect state = state.correctMoves == state.movesGiven

handleAction ::
  forall m.
  Now m =>
  Log m =>
  MonadAff m =>
  MonadEffect m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction Initialize = do
  tagify <- H.liftEffect $ T.mkTagify "input.move-input"
  H.modify_ \state -> state { tagify = Just tagify }

handleAction SubmitSolution = do
  s <- H.get
  answerGiven <- H.liftEffect $ T.getTags s.tagify
  let
    solution = Set.fromFoldable $ maybe [] (\p -> p.squares) s.puzzle

    missed = Set.difference solution answerGiven

    wrong = Set.difference answerGiven solution

    found = Set.intersection solution answerGiven
  state <-
    H.modify \state ->
      state
        { submitted = true
        , correctMoves = solution
        , missedMoves = missed
        , wrongMoves = wrong
        , movesGiven = found
        }
  if isCorrect state && state.settings.moveToNext then
    runDelayedAction NextProblem 1000.0
  else
    pure unit

handleAction NextProblem = do
  state <- H.get
  H.liftEffect $ T.removeAllTags state.tagify
  H.modify_ \s ->
    s
      { submitted = false
      , puzzle = initState.puzzle
      , correctMoves = initState.correctMoves
      , wrongMoves = initState.wrongMoves
      , missedMoves = initState.missedMoves
      }
  H.raise $ GetNextProblem state.settings

handleAction Destroy = do
  state <- H.get
  H.liftEffect Cg.removeResizeListener
  H.liftEffect $ Cg.destroy state.board
  H.modify_ \s -> s { board = Nothing }

handleAction (HandleMoveInputEvent _) = pure unit

handleAction (SettingsChanged settings) = do
  H.modify_ \state -> state { settings = settings }

handleQuery ::
  forall a m.
  MonadEffect m =>
  Now m =>
  Log m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery (RenderProblem q a) = do
  H.modify_ \state -> state { puzzle = Just q.puzzle }
  pure $ Just a
