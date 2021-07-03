module Blind.Component.FindChecks where

import Prelude
import Blind.Capability.Log (logInfo, class Log)
import Blind.Capability.Now (class Now)
import Blind.Component.FFI.ChessGround as C
import Blind.Component.HTML.Utils (css, whenElem)
import Blind.Data.FFI.Tagify as T
import Blind.Data.ProblemInput as ProblemInput
import Blind.Data.Tactics.Puzzle (ChecksPuzzle)
import Data.Set as Set
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.Array as A
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as Ht
import Halogen.HTML.Elements as Elem
import Halogen.HTML.Events as He
import Halogen.HTML.Properties as Hp
import Blind.Component.FFI.ChessGround as Cg
import Blind.Component.Utils (runDelayedAction, moveListCard)

type GameSettings
  = { nbWhite :: Int
    , nbBlack :: Int
    , moveToNext :: Boolean
    , showBoard :: Boolean
    }

defaultSettings :: GameSettings
defaultSettings =
  { nbWhite: 1
  , nbBlack: 2
  , moveToNext: false
  , showBoard: true
  }

data Action
  = Initialize
  | SettingsChanged GameSettings
  | SubmitSolution
  | NextProblem
  | Destroy

type Input
  = String

data Output
  = GetNextProblem { nbWhite :: Int, nbBlack :: Int }
  | GetInitialProblem

data Query a
  = RenderProblem { puzzle :: ChecksPuzzle } a

type State
  = { board :: Maybe C.ChessBoard
    , tagify :: Maybe T.Tagify
    , cfg :: C.BoardConfig
    , submitted :: Boolean
    , puzzle :: ChecksPuzzle
    , results :: Array Int
    , settings :: GameSettings
    , correctMoves :: Set.Set String
    , wrongMoves :: Set.Set String
    , missedMoves :: Set.Set String
    }

initState :: State
initState =
  { board: Nothing
  , tagify: Nothing
  , cfg: initConfig
  , submitted: false
  , puzzle:
      { fen: ""
      , checks: []
      , black: []
      , white: []
      }
  , results: []
  , settings: defaultSettings
  , correctMoves: Set.empty
  , wrongMoves: Set.empty
  , missedMoves: Set.empty
  }

positionDisplay :: forall i p. State -> Ht.HTML i p
positionDisplay state =
  Ht.div_
    [ Ht.h1 [ css "text-center text-capitalize" ] [ Ht.text "white" ]
    , Ht.h5 [ css "text-center font-monospace" ] [ Ht.text $ displayPieces state.puzzle.white ]
    , Ht.h1 [ css "text-center text-capitalize" ] [ Ht.text "black" ]
    , Ht.h5 [ css "text-center font-monospace" ] [ Ht.text $ displayPieces state.puzzle.black ]
    ]

type Slots
  = ( chessboard :: forall query. H.Slot query Void Int )

type BoardState
  = { board :: Maybe C.ChessBoard
    , puzzle :: ChecksPuzzle
    , cfg :: Cg.BoardConfig
    }

data BoardAction
  = InitializeBoard
  | DestroyBoard

boardEl :: String
boardEl = "chess-ground-section"

initConfig :: Cg.BoardConfig
initConfig =
  { fen: ""
  , orientation: ProblemInput.White
  , highlight:
      { lastMove: false
      }
  , viewOnly: false
  , animation:
      { duration: 700
      , enabled: true
      }
  , resisable: true
  , disableContextMenu: true
  , coordinates: false
  , movable:
      { free: false
      }
  , draggable:
      { enabled: false
      }
  , drawable:
      { enabled: true
      , shapes: []
      }
  }

type BoardInput
  = { puzzle :: ChecksPuzzle
    , missedMoves :: Set.Set String
    , wrongMoves :: Set.Set String
    , correctMoves :: Set.Set String
    }

chessboard ::
  forall q o m.
  Now m =>
  Log m => MonadEffect m => H.Component q BoardInput o m
chessboard =
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
  initialBoardState input =
    { board: Nothing
    , puzzle: input.puzzle
    , missedMoves: input.missedMoves
    , wrongMoves: input.wrongMoves
    , correctMoves: input.correctMoves
    , cfg: initConfig { fen = input.puzzle.fen }
    }

  handleBoardAction InitializeBoard = do
    state <- H.get
    let
      missedShapes = A.filter (\x -> Set.member x.move_display state.missedMoves) state.puzzle.checks

      foundShapes = A.filter (\x -> Set.member x.move_display state.correctMoves) state.puzzle.checks

      shapes =
        (map (\x -> { orig: x.from, dest: Just x.to, brush: "green" }) foundShapes)
          <> (map (\x -> { orig: x.from, dest: Just x.to, brush: "red" }) missedShapes)
          <> (map (\x -> { orig: x.to, dest: Nothing, brush: "green" }) foundShapes)
          <> (map (\x -> { orig: x.to, dest: Nothing, brush: "red" }) missedShapes)
    cg <- H.liftEffect $ Cg.mkChessGround boardEl state.cfg
    H.liftEffect Cg.addResizeListener
    H.liftEffect $ Cg.setShapes (Just cg) shapes
    H.modify_ \s -> s { board = Just cg }

  handleBoardAction DestroyBoard = do
    H.liftEffect Cg.removeResizeListener
    state <- H.get
    H.liftEffect $ Cg.destroy state.board
    H.modify_ \s -> s { board = Nothing }
    pure unit

  render _ = Ht.div [ css "blue cburnett" ] [ Ht.div [ Hp.id boardEl ] [] ]

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
                  [ Ht.text "Visualize the position and find all the checks without being captured. Leave empty if no checks are possible."
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
              Ht.slot (Proxy :: _ "chessboard") 0 chessboard
                { puzzle: state.puzzle
                , missedMoves: state.missedMoves
                , wrongMoves: state.wrongMoves
                , correctMoves: state.correctMoves
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
                              , Hp.placeholder "e.g Nh7-f8, Qc3xd4"
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
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> moveListCard state.wrongMoves "Wrong moves")
      , whenElem (state.wrongMoves /= Set.empty) (\_ -> Ht.div [ css "mt-3" ] [])
      , whenElem (state.missedMoves /= Set.empty) (\_ -> moveListCard state.missedMoves "Missed moves")
      ]

displayPieces :: Array String -> String
displayPieces pieces = intercalate " " pieces

puzzleControls :: forall m. State -> Ht.ComponentHTML Action Slots m
puzzleControls state =
  Ht.div_
    [ Ht.label
        [ Hp.attr (Ht.AttrName "for") "number-of-white-pieces", css "form-label"
        ]
        [ Ht.text $ "White pieces: " <> (show state.settings.nbWhite) ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1.0
        , Hp.max 4.0
        , Hp.value (show state.settings.nbWhite)
        , Hp.step (Hp.Step 1.0)
        , Hp.type_ Hp.InputRange
        , Hp.id "number-of-white-pieces"
        , He.onValueChange \v ->
            ( SettingsChanged
                ( state.settings
                    { nbWhite = fromMaybe defaultSettings.nbWhite $ fromString v
                    }
                )
            )
        ]
    , Ht.label
        [ Hp.attr (Ht.AttrName "for") "number-of-black-pieces", css "form-label"
        ]
        [ Ht.text $ "Black pieces: " <> (show state.settings.nbBlack) ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1.0
        , Hp.max 5.0
        , Hp.value (show state.settings.nbBlack)
        , Hp.step (Hp.Step 1.0)
        , Hp.type_ Hp.InputRange
        , Hp.id "number-of-black-pieces"
        , He.onValueChange \v ->
            ( SettingsChanged
                ( state.settings
                    { nbBlack = fromMaybe defaultSettings.nbBlack $ fromString v
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

isCorrect :: State -> Boolean
isCorrect s =
  let
    solution = Set.fromFoldable $ map (\x -> x.move_display) s.puzzle.checks
  in
    s.correctMoves == solution && s.missedMoves == Set.empty && s.wrongMoves == Set.empty

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
    solution = Set.fromFoldable $ map (\x -> x.move_display) s.puzzle.checks

    missed = Set.difference solution answerGiven

    wrong = Set.difference answerGiven solution

    found = Set.intersection solution answerGiven
  state <-
    H.modify \state ->
      state
        { submitted = true
        , correctMoves = found
        , missedMoves = missed
        , wrongMoves = wrong
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
      , results = initState.results
      , correctMoves = initState.correctMoves
      , wrongMoves = initState.wrongMoves
      , missedMoves = initState.missedMoves
      }
  H.raise
    $ GetNextProblem
        { nbWhite: state.settings.nbWhite
        , nbBlack: state.settings.nbBlack
        }

handleAction Destroy = do
  state <- H.get
  H.liftEffect Cg.removeResizeListener
  H.liftEffect $ Cg.destroy state.board
  H.modify_ \s -> s { board = Nothing }

handleAction (SettingsChanged settings) = do
  H.modify_ \state -> state { settings = settings }

handleQuery ::
  forall a m.
  MonadEffect m =>
  Now m =>
  Log m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery (RenderProblem q a) = do
  H.modify_ \state -> state { puzzle = q.puzzle }
  pure $ Just a
