module Blind.Data.FFI.Tagify where

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Prelude

data Tagify

addTags :: Maybe Tagify -> Array String -> Effect Unit
addTags (Just tagify) tags = _addTags tagify tags
addTags Nothing _          = pure unit

getTags :: Maybe Tagify -> Effect (Set.Set String)
getTags (Just tagify) = do
  values <- _getTags tagify
  pure $ Set.fromFoldable values
getTags Nothing       = pure $ Set.empty

getTagsArray :: Maybe Tagify -> Effect (Array String)
getTagsArray (Just tagify) = _getTags tagify
getTagsArray Nothing       = pure []

removeAllTags :: Maybe Tagify -> Effect Unit
removeAllTags (Just tagify) = _removeAllTags tagify
removeAllTags Nothing       = pure unit

destroy :: Maybe Tagify -> Effect Unit
destroy (Just tagify) = _destroy tagify
destroy Nothing       = pure unit

foreign import mkTagify :: String -> Effect Tagify

foreign import _destroy :: Tagify -> Effect Unit

foreign import _addTags :: Tagify -> Array String -> Effect Unit

foreign import _getTags :: Tagify -> Effect (Array String)

foreign import _removeAllTags :: Tagify -> Effect Unit
