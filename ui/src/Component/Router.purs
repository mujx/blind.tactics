module Blind.Component.Router where

import Prelude
import Blind.Capability.Navigate (class Navigate, navigate)
import Blind.Component.Utils (OpaqueSlot)
import Effect.Aff.Class (class MonadAff)
import Blind.Capability.Now (class Now)
import Blind.Capability.Log (class Log)
import Blind.Capability.Resource.TacticsPuzzle (class ManageTacticsPuzzle)
import Blind.Capability.Resource.ChecksPuzzle (class ManageChecksPuzzle)
import Blind.Capability.Resource.KnightPathPuzzle (class ManageKnightPathPuzzle)
import Blind.Capability.Resource.CommonSquaresPuzzle (class ManageCommonSquaresPuzzle)
import Blind.Data.Route (Route(..), routeCodec)
import Blind.Page.Home as Home
import Routing.Duplex (parse)
import Blind.Page.Problem as Problem
import Blind.Page.FindChecks as FindChecks
import Blind.Page.KnightPath as KnightPath
import Blind.Page.CommonSquares as CommonSquares
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as Ht
import Routing.Hash (getHash)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | GoTo Route MouseEvent

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , problem :: OpaqueSlot Unit
    , visualize :: OpaqueSlot Unit
    , findChecks :: OpaqueSlot Unit
    , knightPath :: OpaqueSlot Unit
    , commonSquares :: OpaqueSlot Unit
    )

component ::
  forall i o m.
  MonadAff m =>
  MonadEffect m =>
  Navigate m =>
  ManageTacticsPuzzle m =>
  ManageChecksPuzzle m =>
  ManageKnightPathPuzzle m =>
  ManageCommonSquaresPuzzle m =>
  Now m =>
  Log m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: const { route: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              }
    }

render ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  ManageTacticsPuzzle m =>
  ManageChecksPuzzle m =>
  ManageKnightPathPuzzle m =>
  ManageCommonSquaresPuzzle m =>
  Now m =>
  Log m => State -> H.ComponentHTML Action ChildSlots m
render st = case st.route of
  Nothing -> Ht.h1_ [ Ht.text "Oh no! That page wasn't found" ]
  Just route -> case route of
    Home -> Ht.slot (Proxy :: _ "home") unit Home.component unit absurd
    Visualize -> Ht.slot (Proxy :: _ "visualize") unit Home.component unit absurd
    (Problem _) -> Ht.slot (Proxy :: _ "problem") unit Problem.component unit absurd
    FindChecks -> Ht.slot (Proxy :: _ "findChecks") unit FindChecks.component unit absurd
    KnightPath -> Ht.slot (Proxy :: _ "knightPath") unit KnightPath.component unit absurd
    CommonSquares -> Ht.slot (Proxy :: _ "commonSquares") unit CommonSquares.component unit absurd

handleAction ::
  forall o m.
  MonadEffect m =>
  Navigate m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< parse routeCodec <$> H.liftEffect getHash
    navigate $ fromMaybe Home initialRoute
  GoTo route e -> do
    liftEffect $ preventDefault (toEvent e)
    mRoute <- H.gets _.route
    when (mRoute /= Just route) $ navigate route

handleQuery :: forall a o m. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  -- This is the case that runs every time the brower's hash route changes.
  Navigate route a -> do
    mRoute <- H.gets _.route
    when (mRoute /= Just route) $ H.modify_ _ { route = Just route }
    pure (Just a)
