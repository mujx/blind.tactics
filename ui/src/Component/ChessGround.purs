module Blind.Component.ChessGround where

import Prelude
import Blind.Capability.Log (logDebug, logError, class Log)
import Data.Int (fromString, toNumber, ceil)
import Data.Foldable (foldl, sum)
import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as E
import Blind.Capability.Now (class Now)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Halogen.Query.Event as ES
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML (window) as Web
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Halogen as H
import Halogen.HTML as Ht
import Control.Monad.State.Class (class MonadState)
import Halogen.HTML.Properties as Hp
import Halogen.HTML.Events as He
import Data.Array as A
import Effect.Class (class MonadEffect)
import Math as Math
import Blind.Data.Tactics.Puzzle (Puzzle)
import Blind.Data.ProblemInput as ProblemInput
import Blind.Data.Shape (Shape)
import Blind.Component.HTML.Utils (css, whenElem)
import Blind.Data.FFI.Chartist as Chartist
import Blind.Component.Utils (runDelayedAction)
import Blind.Component.FFI.ChessGround
  ( mkChessGround
  , setConfig
  , setShapes
  , getShapes
  , removeShapes
  , replayMoves
  , BoardConfig
  , ChessBoard
  , destroy
  , addResizeListener
  , removeResizeListener
  , initConfig
  )

type Input
  = ProblemInput.ProblemInput

data Output
  = CorrectSolution { rating :: Int, movesToFollow :: Int }
  | WrongSolution
  | GetInitialProblem

data Query a
  = RenderProblem { problem :: ProblemInput.ProblemInput, puzzle :: Puzzle } a

data MoveStatus
  = Start
  | MoveIndex Int
  | End

derive instance eqMoveState :: Eq MoveStatus

data MoveDirection
  = Up
  | Down

type PuzzleResult
  = { rating :: Number
    , moves :: Int
    , isWin :: Boolean
    }

type State
  = { board :: Maybe ChessBoard
    , progressChart :: Maybe Chartist.Chart
    , cfg :: BoardConfig
    , moves :: Array Shape
    , currentMove :: MoveStatus
    , guess :: Maybe Shape
    , solution :: Maybe Shape
    , errMsg :: Maybe String
    , submitted :: Boolean
    , puzzle :: Maybe Puzzle
    , moveToNext :: Boolean
    , playSolution :: Boolean
    , movesToFollow :: Int
    , rating :: Int
    , playerPerformance :: Number
    , playerRating :: Number
    , ratingHistory :: Array Int
    , puzzleResults :: Array PuzzleResult
    }

initState :: State
initState =
  { board: Nothing
  , progressChart: Nothing
  , cfg: initConfig
  , moves: []
  , currentMove: Start
  , guess: Nothing
  , solution: Nothing
  , errMsg: Nothing
  , submitted: false
  , puzzle: Nothing
  , moveToNext: false
  , playSolution: false
  , movesToFollow: 5
  , rating: 1500
  , playerPerformance: 1500.0
  , playerRating: 1500.0
  , ratingHistory: []
  , puzzleResults: []
  }

isArrowShape :: Shape -> Boolean
isArrowShape s = isJust s.dest

boardEl :: String
boardEl = "chess-ground-section"

startBtn :: forall t1 t2. Ht.HTML t2 t1
startBtn = Ht.button [ css "btn btn-outline-dark", Hp.type_ Hp.ButtonButton ] [ Ht.i [ css "fa fa-step-backward" ] [] ]

endBtn :: forall t1 t2. Ht.HTML t2 t1
endBtn = Ht.button [ css "btn btn-outline-dark", Hp.type_ Hp.ButtonButton ] [ Ht.i [ css "fa fa-step-forward" ] [] ]

prevBtn :: forall t1 t2. Ht.HTML t2 t1
prevBtn = Ht.button [ css "btn btn-outline-dark", Hp.type_ Hp.ButtonButton ] [ Ht.i [ css "fa fa-chevron-left" ] [] ]

nextBtn :: forall t1 t2. Ht.HTML t2 t1
nextBtn = Ht.button [ css "btn btn-outline-dark", Hp.type_ Hp.ButtonButton ] [ Ht.i [ css "fa fa-chevron-right" ] [] ]

sendBtn :: forall t1 t2. Ht.HTML t2 t1
sendBtn = Ht.button [ css "btn btn-dark", Hp.type_ Hp.ButtonButton ] [ Ht.text "SUBMIT" ]

nextProblemBtn :: forall t1 t2. Ht.HTML t2 t1
nextProblemBtn = Ht.button [ css "btn btn-success", Hp.type_ Hp.ButtonButton ] [ Ht.text "NEXT" ]

analysisBtn :: forall t1 t2. Ht.HTML t2 t1
analysisBtn = Ht.button [ css "btn", Hp.type_ Hp.ButtonButton ] [ Ht.text "ANALYZE" ]

playSolutionBtn :: forall t1 t2. Ht.HTML t2 t1
playSolutionBtn = Ht.button [ css "btn btn-primary", Hp.type_ Hp.ButtonButton ] [ Ht.text "SOLUTION" ]

updateMoveStep :: MoveStatus -> MoveDirection -> Int -> MoveStatus
updateMoveStep Start Up n = if n > 0 then MoveIndex 0 else End

updateMoveStep Start Down _ = Start

updateMoveStep End Up _ = End

updateMoveStep End Down n = if n > 0 then MoveIndex (n - 1) else Start

updateMoveStep (MoveIndex i) Up n = if i + 1 >= n then End else MoveIndex (i + 1)

updateMoveStep (MoveIndex i) Down _ = if i - 1 < 0 then Start else MoveIndex (i - 1)

progressChart :: forall t281 t282. Ht.HTML t282 t281
progressChart =
  Ht.div [ css "card" ]
    [ Ht.div [ css "card-header" ] [ Ht.text "Progress" ]
    , Ht.div [ css "card-body", Hp.style "padding: 1rem 0" ]
        [ Ht.div [ css "chart" ] [ Ht.div [ css "ct-chart" ] [] ] ]
    ]

puzzleControls :: forall m. State -> Ht.ComponentHTML Action () m
puzzleControls state =
  Ht.div_
    [ Ht.label
        [ Hp.attr (Ht.AttrName "for") "difficulty-range", css "form-label"
        ]
        [ Ht.text $ "Moves to follow: " <> (show state.movesToFollow) ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1.0
        , Hp.max 20.0
        , Hp.value (show state.movesToFollow)
        , Hp.step (Hp.Step 1.0)
        , Hp.type_ Hp.InputRange
        , Hp.enabled (state.submitted)
        , Hp.id "difficulty-range"
        , He.onValueChange \v -> (MovesToFollowChanged (fromMaybe state.movesToFollow (fromString v)))
        ]
    , Ht.label
        [ Hp.attr (Ht.AttrName "for") "rating-range", css "form-label"
        ]
        [ Ht.text $ "Puzzle rating: [" <> (show $ state.rating - 200) <> " - " <> (show $ state.rating + 200) <> "]" ]
    , Ht.input
        [ css "form-range"
        , Hp.min 1000.0
        , Hp.max 3000.0
        , Hp.step (Hp.Step 200.0)
        , Hp.type_ Hp.InputRange
        , Hp.id "rating-range"
        , Hp.enabled (state.submitted)
        , He.onValueChange \v -> (RatingChanged (fromMaybe state.rating (fromString v)))
        ]
    , Ht.div [ css "form-check form-switch" ]
        [ Ht.input
            [ css "form-check-input"
            , Hp.type_ Hp.InputCheckbox
            , Hp.id "show-next-switch"
            , Hp.value (show state.moveToNext)
            , He.onChecked \v -> (MoveToNextChecked v)
            ]
        , Ht.label [ css "form-check-label", Hp.for "show-next-switch" ] [ Ht.text "Move to next immediately" ]
        ]
    , Ht.div [ css "form-check form-switch" ]
        [ Ht.input
            [ css "form-check-input"
            , Hp.type_ Hp.InputCheckbox
            , Hp.id "play-solution-switch"
            , Hp.value (show state.playSolution)
            , He.onChecked \v -> (ShowSolutionChecked v)
            ]
        , Ht.label [ css "form-check-label", Hp.for "play-solution-switch" ] [ Ht.text "Play the solution when wrong" ]
        ]
    ]

puzzleAttributes :: forall t1 t2. Boolean -> Maybe Puzzle -> Ht.HTML t2 t1
puzzleAttributes _ Nothing = Ht.div_ []

puzzleAttributes isSubmitted (Just puzzle) =
  let
    ratingText = if isSubmitted then show puzzle.rating <> "(±" <> (show puzzle.rating_deviation) <> ")" else "hidden"
  in
    Ht.dd [ css "attributes" ]
      [ Ht.dt_ [ Ht.text "id" ]
      , Ht.dd_ [ Ht.text puzzle.id ]
      , Ht.dt_ [ Ht.text "rating" ]
      , Ht.dd_ [ Ht.text ratingText ]
      , Ht.dt_ [ Ht.text "popularity" ]
      , Ht.dd_ [ Ht.text $ show puzzle.popularity ]
      , Ht.dt_ [ Ht.text "played" ]
      , Ht.dd_ [ Ht.text (show puzzle.nb_plays <> " times") ]
      , Ht.dt_ [ Ht.text "themes" ]
      , Ht.dd_ [ Ht.text (foldl mkBadge "" puzzle.themes) ]
      ]
  where
  mkBadge a b = a <> " #" <> b

calcPerformance :: Array PuzzleResult -> Number
calcPerformance results =
  let
    wins = map (\x -> x.rating + 400.0 + 5.0 * (toNumber x.moves)) $ A.filter (\x -> x.isWin) results

    losses = map (\x -> x.rating - 400.0 + 5.0 * (toNumber x.moves)) $ A.filter (\x -> not x.isWin) results
  in
    (sum wins + sum losses) / (toNumber $ A.length results)

calcElo :: Array Int -> Int -> Boolean -> Number -> Number -> Number
calcElo games movesToFollow isWin userElo puzzleElo =
  let
    numGames = A.length games

    kCo = (if numGames < 10 then 50.0 else 20.0) + 5.0 * (toNumber movesToFollow)

    e0 = 1.0 / (1.0 + (Math.pow 10.0 ((puzzleElo - userElo) / 400.0)))

    s0 = if isWin then 1.0 else 0.0
  in
    userElo + kCo * (s0 - e0)

data Action
  = Initialize
  | Destroy
  | PreviousMove
  | NextMove
  | SubmitSolution
  | NextProblem
  | HandleKey H.SubscriptionId KeyboardEvent
  | MoveToNextChecked Boolean
  | ShowSolutionChecked Boolean
  | MovesToFollowChanged Int
  | RatingChanged Int
  | PlaySolution
  | JumpToStart
  | JumpToEnd

processMove ::
  forall m.
  Bind m =>
  Now m =>
  Log m =>
  MonadEffect m =>
  MonadState State m => MoveStatus -> State -> m Unit
processMove nextMove s = case nextMove of
  Start -> do
    H.liftEffect $ setShapes s.board []
    H.modify_ \state ->
      state
        { currentMove = nextMove
        , errMsg = Nothing
        , guess = if nextMove /= End then Nothing else s.guess
        }
  MoveIndex idx -> do
    case s.board of
      Just b -> case A.index s.moves idx of
        Just elem -> H.liftEffect $ setShapes (Just b) [ elem ]
        Nothing -> pure unit
      Nothing -> do
        logError "board was not initialized"
        pure unit
    H.modify_ \state ->
      state
        { currentMove = nextMove
        , errMsg = Nothing
        , guess = if nextMove /= End then Nothing else s.guess
        }
  End -> do
    H.liftEffect $ setShapes s.board []
    H.modify_ \state ->
      state
        { currentMove = nextMove
        , errMsg = Nothing
        , guess = if nextMove /= End then Nothing else s.guess
        }

component ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  Now m =>
  Log m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
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
  initialState :: Input -> State
  initialState input =
    initState
      { cfg =
        initConfig
          { fen = input.fen
          , orientation = input.orientation
          }
      , moves = input.moves
      , solution = Just input.solution
      }

  handleQuery ::
    forall a k.
    MonadEffect k =>
    Now k =>
    Log k => Query a -> H.HalogenM State Action () Output k (Maybe a)
  handleQuery (RenderProblem q a) = do
    logDebug "rendering a new problem"
    newState <-
      H.modify \state ->
        state
          { cfg =
            state.cfg
              { fen = q.problem.fen
              , orientation = q.problem.orientation
              , animation = initConfig.animation { duration = 0 }
              }
          , moves = q.problem.moves
          , solution = Just q.problem.solution
          , submitted = false
          , errMsg = Nothing
          , guess = Nothing
          , currentMove = Start
          , puzzle = Just q.puzzle
          }
    H.liftEffect $ setShapes newState.board []
    H.liftEffect $ setConfig newState.board newState.cfg
    H.liftEffect $ setConfig newState.board (newState.cfg { animation = initConfig.animation })
    pure $ Just a

  handleAction ::
    forall m1.
    Now m1 =>
    Log m1 =>
    MonadAff m1 =>
    MonadEffect m1 => Action -> H.HalogenM State Action () Output m1 Unit
  handleAction Initialize = do
    state <- H.get
    cg <- H.liftEffect $ mkChessGround boardEl state.cfg
    chart <- H.liftEffect $ Chartist.mkLineChart ".ct-chart" []
    H.liftEffect addResizeListener
    H.modify_ \s -> s { board = Just cg, progressChart = Just chart }
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  handleAction (MoveToNextChecked v) = H.modify_ \s -> s { moveToNext = v }

  handleAction (ShowSolutionChecked v) = H.modify_ \s -> s { playSolution = v }

  handleAction (MovesToFollowChanged v) = H.modify_ \s -> s { movesToFollow = v }

  handleAction (RatingChanged v) = H.modify_ \s -> s { rating = v }

  handleAction (HandleKey _ ev) = case KE.key ev of
    "ArrowRight" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      state <- H.get
      processMove (updateMoveStep state.currentMove Up (A.length state.moves)) state
    "ArrowLeft" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      state <- H.get
      processMove (updateMoveStep state.currentMove Down (A.length state.moves)) state
    _ -> pure unit

  handleAction PreviousMove = do
    state <- H.get
    processMove (updateMoveStep state.currentMove Down (A.length state.moves)) state

  handleAction NextMove = do
    state <- H.get
    processMove (updateMoveStep state.currentMove Up (A.length state.moves)) state

  handleAction NextProblem = do
    state <- H.get
    H.raise
      $ CorrectSolution
          { rating: state.rating
          , movesToFollow: state.movesToFollow
          }

  handleAction JumpToStart = do
    state <- H.get
    H.liftEffect $ setConfig state.board state.cfg
    H.modify_ \s -> s { errMsg = Nothing, currentMove = Start }

  handleAction JumpToEnd = do
    state <- H.get
    H.liftEffect $ setConfig state.board state.cfg
    H.modify_ \s -> s { errMsg = Nothing, currentMove = End }

  handleAction PlaySolution = do
    state <- H.get
    H.liftEffect $ removeShapes state.board
    H.liftEffect $ replayMoves state.board (state.moves <> (maybe [] (\x -> [ x ]) state.solution))

  handleAction SubmitSolution = do
    state <- H.get
    case state.board of
      Just b -> do
        let
          shapes = A.filter isArrowShape $ getShapes b
        if A.null shapes then
          H.modify_ \s -> s { errMsg = Just "No moves were given" }
        else if A.length shapes > 1 then
          H.modify_ \s -> s { errMsg = Just "More than one moves were given" }
        else
          let
            currentGuess = A.head shapes
          in
            case currentGuess of
              Nothing -> logError "no moves were found"
              Just _ -> do
                let
                  puzzleRating = maybe 1500 (\p -> p.rating) state.puzzle

                  isWin = state.solution == currentGuess

                  newPlayerRating =
                    calcElo
                      state.ratingHistory
                      state.movesToFollow
                      isWin
                      state.playerRating
                      (toNumber puzzleRating)

                  ratingHistory = A.snoc state.ratingHistory (ceil newPlayerRating)

                  puzzleResults = A.snoc state.puzzleResults ({ rating: toNumber puzzleRating, moves: state.movesToFollow, isWin: isWin })
                H.modify_ \state' ->
                  state'
                    { guess = currentGuess
                    , errMsg = Nothing
                    , submitted = true
                    , ratingHistory = ratingHistory
                    , playerRating = newPlayerRating
                    , puzzleResults = puzzleResults
                    , playerPerformance = calcPerformance puzzleResults
                    }
                H.liftEffect $ Chartist.updateChart state.progressChart ratingHistory
                if isWin then
                  if state.moveToNext then
                    runDelayedAction NextProblem 500.0
                  else
                    pure unit
                else if state.playSolution then
                  runDelayedAction PlaySolution 1000.0
                else
                  pure unit
      Nothing -> logError "cannot submit solution on an uninitialized board"

  handleAction Destroy = do
    H.liftEffect removeResizeListener
    state <- H.get
    H.liftEffect $ destroy state.board
    H.liftEffect $ Chartist.destroyChart state.progressChart
    H.modify_ \s -> s { board = Nothing }
    pure unit

  render state =
    Ht.div [ css "d-flex chessground-wrapper" ]
      [ Ht.aside [ css "col-3 col-md-3 d-flex flex-column px-4 left-panel" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Instructions" ]
              , Ht.div [ css "card-body" ]
                  [ Ht.text
                      "Use the arrow controls to move through the puzzle. Visualize the resulting position \
              \and try to solve it. Draw your move on the board and click submit to check your solution."
                  ]
              ]
          , Ht.div [ css "mt-4" ] []
          , Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Problem" ]
              , Ht.div [ css "card-body" ] [ puzzleAttributes state.submitted state.puzzle ]
              ]
          , Ht.div [ css "mt-4" ] []
          , Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Settings" ]
              , Ht.div [ css "card-body" ] [ puzzleControls state ]
              ]
          ]
      , Ht.div [ css "col-6" ]
          [ Ht.div [ css "d-flex flex-column", Hp.id "chessboard" ]
              [ chessBoard
              , boardControls state
              ]
          ]
      , Ht.aside [ css "col-3 d-flex flex-column flex-fill px-3" ]
          [ Ht.div [ css "card" ]
              [ Ht.div [ css "card-header" ] [ Ht.text "Your stats" ]
              , Ht.div [ css "card-body" ]
                  [ Ht.dd [ css "attributes" ]
                      [ Ht.dt_ [ Ht.text "Rating" ]
                      , Ht.dd_ [ Ht.text $ show $ ceil state.playerRating ]
                      , Ht.dt_ [ Ht.text "Performance" ]
                      , Ht.dd_ [ Ht.text $ show $ ceil state.playerPerformance ]
                      ]
                  ]
              ]
          , Ht.div [ css "mt-4" ] []
          , progressChart
          , Ht.div [ css "mt-4" ] []
          , whenElem (isJust state.errMsg) (const $ errField state.errMsg)
          , whenElem (isJust state.guess && not isJust state.errMsg) (const $ resultField state.guess state.solution)
          ]
      ]
    where
    boardControls s' =
      Ht.div [ css "d-flex flex-row justify-content-between mt-3" ]
        [ Ht.div [ css "d-flex" ]
            [ btnControls
            ]
        , whenElem
            (s'.currentMove == End && not s'.submitted)
            (\_ -> Ht.div [ css "d-flex" ] [ submitBtn ])
        , whenElem
            (s'.submitted)
            (\_ -> Ht.div [ css "d-flex" ] [ analysisButton s'.puzzle, playSolutionButton, nextProblemButton ])
        ]

    resultField guess solution =
      if guess == solution then
        Ht.div [ css "alert alert-success" ]
          [ Ht.span_ [ Ht.i [ css "fa fa-check", Hp.style "margin-right: 8px" ] [], Ht.text "Correct!" ]
          ]
      else
        Ht.div [ css "alert alert-danger" ]
          [ Ht.span_ [ Ht.i [ css "fa fa-times", Hp.style "margin-right: 8px" ] [], Ht.text "Wrong move!" ]
          ]

    errField Nothing = Ht.text ""

    errField (Just msg) = Ht.div [ css "alert alert-warning" ] [ Ht.text msg ]

    chessBoard = Ht.div [ css "blue cburnett" ] [ Ht.div [ Hp.id boardEl ] [] ]

    btnControls =
      Ht.div
        [ css "d-flex justify-content-center" ]
        [ whenElem (state.submitted) (\_ -> Ht.div [ He.onClick \_ -> JumpToStart ] [ startBtn ])
        , Ht.div [ css "ms-1", He.onClick \_ -> PreviousMove ] [ prevBtn ]
        , Ht.div [ css "ms-1", He.onClick \_ -> NextMove ] [ nextBtn ]
        , whenElem (state.submitted) (\_ -> Ht.div [ css "ms-1", He.onClick \_ -> JumpToEnd ] [ endBtn ])
        ]

    submitBtn = Ht.div [ He.onClick \_ -> SubmitSolution ] [ sendBtn ]

    nextProblemButton = Ht.div [ css "ms-1", He.onClick \_ -> NextProblem ] [ nextProblemBtn ]

    playSolutionButton = Ht.div [ css "ms-1", He.onClick \_ -> PlaySolution ] [ playSolutionBtn ]

    analysisButton p = case p of
      Nothing -> Ht.div_ []
      Just puzzle -> Ht.a [ Hp.href puzzle.game_url, Hp.attr (Ht.AttrName "target") "_blank" ] [ analysisBtn ]
