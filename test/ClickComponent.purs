module ClickComponent where

  import Control.Monad.Free
  import Control.Monad.Rec.Class
  import Control.Monad.State.Class (modify)
  import Control.Monad.State.Trans

  import Data.Coyoneda
  import Data.Identity
  import Data.Inject
  import Data.Void

  import HalogenC
  import qualified Halogen.HTML as H

  data Input a = ClickIncrement a | ClickDecrement a

  clickIncrement :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g Unit
  clickIncrement = liftF (inj (liftCoyoneda $ ClickIncrement unit) :: g Unit)

  clickDecrement :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g Unit
  clickDecrement = liftF (inj (liftCoyoneda $ ClickDecrement unit) :: g Unit)

  counterComponent :: forall g. (MonadRec g) => ComponentFC Number Input g Void
  counterComponent = component render query
    where

    eval :: forall g. (Monad g) => Natural Input (StateT Number g)
    eval (ClickIncrement next) = do
      modify (+1)
      return next
    eval (ClickDecrement next) = do
      modify (flip (-) 1)
      return next

    render :: Number -> H.HTML Void (FreeC Input Unit)
    render n = H.text (show n)

    query :: forall g i. (MonadRec g) => FreeC Input i -> StateT Number g i
    query = runFreeCM eval

  test :: FreeC Input Unit
  test = do
    clickIncrement
    clickIncrement
    clickDecrement
