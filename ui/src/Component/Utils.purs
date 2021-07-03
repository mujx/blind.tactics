module Blind.Component.Utils where

import Prelude
import Halogen as H
import Halogen.HTML as Ht
import Effect.Class (class MonadEffect)
import Effect
import Web.HTML (window)
import Web.HTML.Window (location, document)
import Web.HTML.HTMLDocument (setTitle)
import Effect.Aff.Class (class MonadAff)
import Halogen.Subscription as HS
import Data.Set as Set
import Effect.Aff as Aff
import Effect.Aff (Milliseconds(..))
import Blind.Component.HTML.Utils (css)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot
  = forall query. H.Slot query Void slot

-- | Sometimes it's useful for a component to subscribe to a stream of incoming information.
-- | Halogen provides 'event sources' for this purpose. For example, you can send messages to
-- | subscribers when key events occur on the global window, so multiple components subscribe to
-- | and are notified about these events.
-- |
-- | At other times it's useful to subscribe to non-DOM events. The most common of these is when
-- | you have a global state with a piece of mutable data and multiple components need to stay in
-- | sync about the current value of that data. Each time the data is changed, you can broadcast
-- | the change to subscribed components so they always have the correct information.
-- |
-- | In our case, we'll use this to subscribe components to updates about the value of the current
-- | user in global state.
-- |
-- | This helper function helps create an event source from a many-to-many bus. For example:
-- |
-- | ```purescript
-- | handleAction = case _ of
-- |   Initialize -> do
-- |     { currentUser, userBus } <- ask
-- |     _ <- H.subscribe $ busEventSource $ HandleBus <$> userBus
-- |     mbProfile <- liftEffect $ Ref.read currentUser
-- |     ...
-- |
-- |   HandleBus busMessage -> do
-- |     ...
-- | ```
-- busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> Es.EventSource m act
-- busEventSource bus =
--   Es.affEventSource \emitter -> do
--     fiber <- forkAff $ forever $ Es.emit emitter =<< Bus.read bus
--     pure (Es.Finalizer (killFiber (error "Event source closed") fiber))
runDelayedAction ::
  forall state slots output m a.
  MonadEffect m =>
  MonadAff m => a -> Number -> H.HalogenM state a slots output m Unit
runDelayedAction action delay = do
  { emitter, listener } <- H.liftEffect HS.create
  void $ H.subscribe emitter
  H.liftAff $ Aff.delay (Milliseconds delay)
  H.liftEffect $ HS.notify listener action

moveListCard :: forall i p. Set.Set String -> String -> Ht.HTML i p
moveListCard moves title =
  let
    groupList = map (\x -> Ht.li [ css "list-group-item" ] [ Ht.text x ]) $ Set.toUnfoldable moves
  in
    Ht.div [ css "card" ]
      [ Ht.div [ css "card-header" ] [ Ht.text title ]
      , Ht.div [ css "card-body" ]
          [ Ht.div [ css "d-flex flex-column" ]
              [ Ht.ul [ css "list-group list-group-flush scroll-md" ] groupList
              ]
          ]
      ]

setWindowTitle :: String -> Effect Unit
setWindowTitle t = do
  w <- H.liftEffect window
  doc <- H.liftEffect $ document w
  H.liftEffect $ setTitle t doc
