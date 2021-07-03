module Blind.AppM where

import Blind.Capability.Navigate (class Navigate)
import Blind.Capability.Log (class Log)
import Blind.Capability.Resource.TacticsPuzzle (class ManageTacticsPuzzle)
import Blind.Capability.Resource.ChecksPuzzle (class ManageChecksPuzzle)
import Blind.Capability.Resource.KnightPathPuzzle (class ManageKnightPathPuzzle)
import Blind.Capability.Resource.CommonSquaresPuzzle (class ManageCommonSquaresPuzzle)
import Blind.Api.Request as Req
import Blind.Capability.Now (class Now)
import Effect.Now as Now
import Blind.Data.Route (routeCodec)
import Blind.Env (Env, LogLevel(..))
import Blind.Data.LogItem as LogItem
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude
import Routing.Hash (setHash)
import Routing.Duplex (print)
import Effect.Console as Console
import Type.Equality (class TypeEquals, from)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec

instance logMessagesAppM :: Log AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, LogItem.reason log of
      Prod, LogItem.Debug -> pure unit
      _, _ -> Console.log $ LogItem.message log

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- | Our operations for managing tactics puzzles.
instance manageTacticsPuzzleAppM :: ManageTacticsPuzzle AppM where
  getNextPuzzle rating movesToFollow = do
    env <- ask
    Req.getNextTacticsPuzzle (env.baseUrl) rating movesToFollow

-- | Our operations for managing checks puzzles.
instance manageChecksPuzzleAppM :: ManageChecksPuzzle AppM where
  getNextPuzzle opts = do
    env <- ask
    Req.getNextChecksPuzzle (env.baseUrl) opts

-- | Our operations for managing knight path puzzles.
instance manageKnightPathPuzzleAppM :: ManageKnightPathPuzzle AppM where
  getNextPuzzle opts = do
    env <- ask
    Req.getNextKnightPathPuzzle (env.baseUrl) opts

-- | Our operations for managing common squares puzzles.
instance manageCommonSquaresPuzzleAppM :: ManageCommonSquaresPuzzle AppM where
  getNextPuzzle opts = do
    env <- ask
    Req.getNextCommonSquaresPuzzle (env.baseUrl) opts
