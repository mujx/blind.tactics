module Blind.Capability.Resource.KnightPathPuzzle where

import Prelude
import Blind.Data.Tactics.Puzzle (KnightPathPuzzle)
import Data.Either (Either)
import Halogen (HalogenM, lift)

class Monad m <= ManageKnightPathPuzzle m where
  getNextPuzzle :: forall r. { maxSteps :: Int | r } -> m (Either String KnightPathPuzzle)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageTacticsPuzzleHalogenM :: ManageKnightPathPuzzle m => ManageKnightPathPuzzle (HalogenM st act slots msg m) where
  getNextPuzzle opts = lift $ getNextPuzzle opts
