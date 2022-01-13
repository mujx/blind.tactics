module Blind.Component.KnightPath where

import Prelude
import Blind.Capability.Log (class Log)
import Blind.Capability.Now (class Now)
import Blind.Component.HTML.Utils (css, whenElem)
import Blind.Data.Tactics.Puzzle (KnightPathPuzzle)
import Blind.Component.Utils (moveListCard, runDelayedAction)
import Data.Int (fromString)
import Data.Set as Set
import Data.Foldable (maximum)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.Query as Hq
import Halogen.HTML as Ht
import Halogen.HTML.Events as He
import Halogen.HTML.Properties as Hp
import Blind.Component.MoveInput as MoveInput
import Data.String.Common (joinWith)
import Blind.Component.ChessBoard as ChessBoard

type Slots
  = ( moveInput :: H.Slot MoveInput.Query MoveInput.Output Int
    , chessboard :: forall query. H.Slot query Void Int
    )

type GameSettings
  = { maxSteps :: Int
    , moveToNext :: Boolean
    , showBoard :: Boolean
    }

defaultSettings :: GameSettings
defaultSettings =
  { maxSteps: 2
  , moveToNext: false
  , showBoard: false
  }

data Action
  = Initialize
  | SettingsChanged GameSettings
  | SubmitSolution
  | HandleMoveInputEvent MoveInput.Output
  | NextProblem
  | Destroy

type Input
  = String

data Output
  = GetNextProblem GameSettings
  | GetInitialProblem

data Query a
  = RenderProblem { puzzle :: KnightPathPuzzle } a

type State
  = { tagComponentIds :: Array Int
    , submitted :: Boolean
    , puzzle :: Maybe KnightPathPuzzle
    , settings :: GameSettings
    , correctMoves :: Set.Set String
    , movesGiven :: Set.Set String
    , wrongMoves :: Set.Set String
    , missedMoves :: Set.Set String
    }

initState :: State
initState =
  { tagComponentIds: []
  , submitted: false
  , puzzle: Nothing
  , settings: defaultSettings
  , correctMoves: Set.empty
  , movesGiven: Set.empty
  , wrongMoves: Set.empty
  , missedMoves: Set.empty
  }

_moveInput = Proxy :: Proxy "moveInput"

computeNextIndex :: Array Int -> Int
computeNextIndex arr = case maximum arr of
  Just v -> v + 1
  Nothing -> 0

problemStatement :: forall i p. State -> Ht.HTML i p
problemStatement state =
  Ht.div_
    [ Ht.h1 [ css "text-center font-monospace" ] [ Ht.text $ fmtSquares state ]
    ]
  where
  fmtSquares s' = maybe "" (\p -> p.from <> " ⟹   " <> p.to) s'.puzzle

mkTagInputs ::
  forall m t.
  MonadAff m =>
  Array Int ->
  Ht.HTML
    ( H.ComponentSlot
        ( moveInput :: H.Slot MoveInput.Query MoveInput.Output Int
        | t
        )
        m
        Action
    )
    Action
mkTagInputs componentIds =
  Ht.div [ css "d-flex flex-column" ]
    ( A.concatMap
        ( \idx ->
            [ Ht.slot _moveInput idx MoveInput.component
                { index: idx
                , placeholderText: "e.g b4, c2, e1"
                }
                HandleMoveInputEvent
            , Ht.div [ css "m-1" ] []
            ]
        )
        componentIds
    )

isCorrect :: State -> Boolean
isCorrect s = s.movesGiven == s.correctMoves

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
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> moveListCard state.wrongMoves "Wrong paths")
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> Ht.div [ css "mt-3" ] [])
      , whenElem (state.missedMoves /= Set.empty) (\_ -> moveListCard state.missedMoves "Missed paths")
      ]

puzzleControls :: forall m. State -> Ht.ComponentHTML Action Slots m
puzzleControls state =
  Ht.div_
    [ Ht.label
        [ Hp.attr (Ht.AttrName "for") "max-knight-steps", css "form-label"
        ]
        [ Ht.text $ "Max steps: " <> (show state.settings.maxSteps) ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1.0
        , Hp.max 5.0
        , Hp.value (show state.settings.maxSteps)
        , Hp.step (Hp.Step 1.0)
        , Hp.type_ Hp.InputRange
        , Hp.id "max-knight-steps"
        , He.onValueChange \v ->
            ( SettingsChanged
                ( state.settings
                    { maxSteps = fromMaybe defaultSettings.maxSteps $ fromString v
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

component ::
  forall m.
  MonadAff m =>
  Now m =>
  Log m =>
  MonadEffect m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: const initState
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
  handleAction ::
    forall m1.
    Now m1 =>
    Log m1 =>
    MonadAff m1 =>
    MonadEffect m1 => Action -> H.HalogenM State Action Slots Output m1 Unit
  handleAction Initialize = do
    H.modify_ \state -> state { tagComponentIds = [ computeNextIndex [] ] }

  handleAction SubmitSolution = do
    s <- H.get
    paths <- Hq.requestAll _moveInput MoveInput.GetTags
    let
      solution = case s.puzzle of
        Just puzzle -> puzzle.squares
        Nothing -> []

      correctMoves = Set.fromFoldable $ map (joinWith " - ") solution

      movesGiven = Set.fromFoldable $ map (joinWith " - ") paths

      wrongMoves = Set.difference movesGiven correctMoves

      missedMoves = Set.difference correctMoves movesGiven
    state <-
      H.modify \state ->
        state
          { submitted = true
          , correctMoves = correctMoves
          , movesGiven = movesGiven
          , wrongMoves = Set.filter (\x -> x /= "") wrongMoves
          , missedMoves = Set.filter (\x -> x /= "") missedMoves
          }
    if isCorrect state && state.settings.moveToNext then
      runDelayedAction NextProblem 1000.0
    else
      pure unit

  handleAction (HandleMoveInputEvent event) = case event of
    MoveInput.Add _ -> do
      H.modify_ \state ->
        state
          { tagComponentIds =
            A.cons
              (computeNextIndex state.tagComponentIds)
              state.tagComponentIds
          }
    MoveInput.Delete idx -> do
      s <- H.get
      if A.length s.tagComponentIds > 1 then
        H.modify_ \state -> state { tagComponentIds = A.filter (\x -> x /= idx) state.tagComponentIds }
      else
        pure unit

  handleAction NextProblem = do
    currentState <- H.get
    _ <- Hq.tellAll _moveInput MoveInput.RemoveTags
    H.modify_
      ( \_ ->
          initState
            { settings = currentState.settings
            , tagComponentIds = [ computeNextIndex [] ]
            }
      )
    H.raise $ GetNextProblem currentState.settings

  handleAction Destroy = pure unit

  handleAction (SettingsChanged settings) = H.modify_ \s -> s { settings = settings }

  handleQuery ::
    forall a m'.
    MonadEffect m =>
    Now m' =>
    Log m' => Query a -> H.HalogenM State Action Slots Output m' (Maybe a)
  handleQuery (RenderProblem q a) = do
    H.modify_ \state -> state { puzzle = Just q.puzzle }
    pure $ Just a

  render state =
    Ht.div [ css "d-flex find-the-checks-wrapper" ]
      [ Ht.aside [ css "col-3 d-flex flex-column px-4 left-panel" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Instructions" ]
              , Ht.div [ css "card-body" ]
                  [ Ht.text "Find all the shortest paths to bring a knight from the start to the end square."
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
                , shapes: Set.empty
                }
                absurd
            else
              problemStatement state
          ]
      , Ht.aside [ css "col-3 d-flex flex-column flex-fill px-4" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Solution" ]
              , Ht.div [ css "card-body" ]
                  [ mkTagInputs state.tagComponentIds
                  , Ht.div [ css "mt-4" ] []
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
