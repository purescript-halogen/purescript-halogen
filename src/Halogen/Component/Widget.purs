module Halogen.Component.Widget
  ( WidgetF()
  , WidgetFC()
  , WidgetHandler()
  , WidgetInput()
  , widgetF
  , widgetF_
  , widgetFC
  , widgetFC_
  , liftEffW
  ) where

import Prelude

import Control.Monad.Free (Free(), liftFI)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)

import Data.Coyoneda (Coyoneda(), liftCoyonedaTF)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct)

import Halogen
import Halogen.Query.StateF (StateF())
import qualified Halogen.HTML as H

type WidgetF s f g p = ComponentF s (Coproduct WidgetInput f) g p
type WidgetFC s f g p = ComponentF s (Coproduct WidgetInput (Coyoneda f)) g p

type WidgetHandler s g = HTMLElement -> Free (Coproduct (StateF s) g) Unit

data WidgetInput a
  = Init HTMLElement a
  | Destroy HTMLElement a

instance functorWidgetInput :: Functor WidgetInput where
  map f (Init el a) = Init el (f a)
  map f (Destroy el a) = Init el (f a)

widgetF :: forall s f g p. (Functor f, Functor g) => WidgetHandler s g -> WidgetHandler s g -> Eval f s g -> WidgetF s f g p
widgetF init destroy evalF = componentF render (coproduct (evalW init destroy) evalF)

widgetF_ :: forall s f g p. (Functor f, Applicative g) => WidgetHandler s g -> Eval f s g -> WidgetF s f g p
widgetF_ init evalF = componentF render (coproduct (evalW init nullDestroy) evalF)

widgetFC :: forall s f g p. (Functor g) => WidgetHandler s g -> WidgetHandler s g -> Eval f s g -> WidgetFC s f g p
widgetFC init destroy evalF = componentF render (coproduct (evalW init destroy) (liftCoyonedaTF evalF))

widgetFC_ :: forall s f g p. (Applicative g) => WidgetHandler s g -> Eval f s g -> WidgetFC s f g p
widgetFC_ init evalF = componentF render (coproduct (evalW init nullDestroy) (liftCoyonedaTF evalF))

render :: forall s p f. (Functor f) => RenderF s p (Coproduct WidgetInput f)
render = const $ H.div [ H.Initializer (\el -> actionF (Init el))
                       , H.Finalizer (\el -> actionF (Destroy el))
                       ]
                       []

evalW :: forall s g. (Functor g) => WidgetHandler s g -> WidgetHandler s g -> Eval WidgetInput s g
evalW f _ (Init el next) = f el $> next
evalW _ f (Destroy el next) = f el $> next

nullDestroy :: forall s g. (Applicative g) => WidgetHandler s g
nullDestroy = const (pure unit)

-- | Interacting with the DOM parts of widgets will usually be done via `Eff`,
-- | but we tend to operate in `Aff` when using Halogen, so this function helps
-- | lift an `Eff` action into an `Aff` or any other
-- | `MonadEff`-instance-providing-type when working with widgets.
-- |
-- | This is identical to `liftFI <<< liftEff`, but typed in such a way that
-- | code using `liftEffW` won't require decorating with explicit type
-- | signatures.
liftEffW :: forall eff g a s. (MonadEff eff g, Functor g) => Eff eff a -> Free (Coproduct (StateF s) g) a
liftEffW e = liftFI (liftEff e :: g a)
