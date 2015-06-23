module HalogenC where

  import Control.Monad.Free
  import Control.Monad.State.Trans
  import Halogen.HTML

  newtype Component s f g p = Component
    { render :: s -> HTML p (f Unit)
    , query  :: forall i. Free f i -> StateT s g i
    }

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
  type InputC = Coyoneda Input

  clickIncrement :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g Unit
  clickIncrement = liftF (inj (liftCoyoneda $ ClickIncrement unit) :: g Unit)

  clickDecrement :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g Unit
  clickDecrement = liftF (inj (liftCoyoneda $ ClickDecrement unit) :: g Unit)

  counterComponent :: forall g. (MonadRec g) => Component Number InputC g Void
  counterComponent = Component { render : render, query : query }
    where

    eval :: forall g. (Monad g) => Natural Input (StateT Number g)
    eval (ClickIncrement next) = do
      modify (+1)
      return next
    eval (ClickDecrement next) = do
      modify (flip (-) 1)
      return next

    render :: Number -> H.HTML Void (InputC Unit)
    render n = H.text (show n)

    query :: forall g i. (MonadRec g) => Free InputC i -> StateT Number g i
    query = runFreeCM eval

  test :: Free InputC Unit
  test = do
    clickIncrement
    clickIncrement
    clickDecrement
