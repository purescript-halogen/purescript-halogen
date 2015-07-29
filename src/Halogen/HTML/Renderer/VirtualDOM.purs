module Halogen.HTML.Renderer.VirtualDOM
  ( RenderState()
  , IFMemoRecord()
  , MemoBox()
  , renderHTML
  , emptyRenderState
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.State (State(), runState)
import Control.Monad.State.Class (gets, modify)

import Data.Coyoneda (Natural())
import Data.DOM.Simple.Types (HTMLElement())
import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldl, fold, find)
import Data.Function (runFn2)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Nullable (toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)

import Halogen.Effects (HalogenEffects())
import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), HandlerF(..), runNamespace, runTagName, runPropName, runAttrName, runEventName)
import Halogen.HTML.Events.Handler (runEventHandler)
import qualified Halogen.Internal.VirtualDOM as V

type RenderState =
  { initializers :: List IFMemoRecord
  , finalizers :: List IFMemoRecord
  }

emptyRenderState :: RenderState
emptyRenderState = { initializers: mempty, finalizers: mempty }

-- | A memoized initializer or finalizer function - a `MemoBox`-wrapped handler
-- | function with its associated virtual-dom property object.
type IFMemoRecord = Tuple MemoBox V.Props

-- | A type for wrapping initializer/finalizer functions in such a way that
-- | their equality can be tested for memoization purposes. By design the only
-- | useful operation for this type is `eq`.
newtype MemoBox = MemoBox (forall i. HTMLElement -> i)

instance eqMemoBox :: Eq MemoBox where
  eq = _eqMemoBox

foreign import _eqMemoBox :: MemoBox -> MemoBox -> Boolean

-- | This horrendously unsafe function is "safe" because `MemoBox` is a newtype,
-- | which has no runtime representation, and the function is never going to be
-- | used, so the existential does not need wrapping in a way that is
-- | unpackable.
mkMemoBox :: forall i. (HTMLElement -> i) -> MemoBox
mkMemoBox = Unsafe.Coerce.unsafeCoerce

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML :: forall p f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> HTML p (f Unit) -> RenderState -> Tuple V.VTree RenderState
renderHTML f html st = runState (go html) emptyRenderState
  where
  go :: HTML p (f Unit) -> State RenderState V.VTree
  go (Text s) = pure $ V.vtext s
  go (Element ns name props els) = do
      let ns' = toNullable $ runNamespace <$> ns
          tag = runTagName name
          key = toNullable $ foldl findKey Nothing props
      V.vnode ns' tag key <$> (fold <$> traverse (renderProp f st) props)
                          <*> traverse go els
  go (Placeholder _) = pure $ V.vtext ""

renderProp :: forall f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> RenderState -> Prop (f Unit) -> State RenderState V.Props
renderProp _ _ (Prop e) = pure $ runExists (\(PropF key value _) ->
  runFn2 V.prop (runPropName key) value) e
renderProp _ _ (Attr ns name value) =
  let attrName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runAttrName name
  in pure $ runFn2 V.attr attrName value
renderProp dr _ (Handler e) = pure $ runExistsR (\(HandlerF name k) ->
  runFn2 V.handlerProp (runEventName name) \ev -> handleAff $ runEventHandler ev (k ev) >>= dr) e
renderProp dr st (Initializer f) = ifprop st (_.initializers) (\is -> _ { initializers = is }) V.initProp dr f
renderProp dr st (Finalizer f) = ifprop st (_.finalizers) (\fs -> _ { finalizers = fs }) V.finalizerProp dr f
renderProp _ _ _ = pure mempty

-- | Creates a initializer/finalizer if required, or returns a previously
-- | memoized version if one exists.
-- |
-- | Even though the previous `RenderState` is passed in to perform the lookup,
-- | we also need to check the currently accumulating `RenderState` - if an
-- | initializer/finalizer is used in more than one place in the current HTML
-- | structure we don't want to clobber previously memoized versions of it in
-- | the current render cycle.
ifprop :: forall eff f i. RenderState
                     -> (RenderState -> List IFMemoRecord)
                     -> (List IFMemoRecord -> RenderState -> RenderState)
                     -> ((HTMLElement -> Eff (HalogenEffects eff) Unit) -> V.Props)
                     -> (f Unit -> Aff (HalogenEffects eff) Unit)
                     -> (HTMLElement -> f Unit)
                     -> State RenderState V.Props
ifprop oldState getter modifier mkProp dr f = do
  fs <- gets getter
  let fbox = mkMemoBox f
  case find (eq fbox <<< fst) (getter oldState) <|> find (eq fbox <<< fst) fs of
    Nothing -> do
      let prop = mkProp (handleAff <<< dr <<< f)
      modify (modifier (Tuple fbox prop : fs))
      pure prop
    Just (Tuple _ prop) -> pure prop

handleAff :: forall eff a. Aff (HalogenEffects eff) a -> Eff (HalogenEffects eff) Unit
handleAff = runAff throwException (const (pure unit))

findKey :: forall i. Maybe String -> Prop i -> Maybe String
findKey _ (Key k) = Just k
findKey r _ = r
