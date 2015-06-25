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

module EditorComponent where

  import Control.Monad.State.Trans
  import Control.Monad.State.Class (modify)
  import Control.Monad.Free
  import Control.Monad.Rec.Class
  import Control.Monad.Eff
  import Control.Monad.Trans (lift)
  import Control.Apply ((*>))

  import Data.Coyoneda
  import Data.Inject
  import Data.Void
  import Data.Identity

  import DOM (DOM())
  import HalogenC
  import qualified Halogen.HTML as H

  data Input a = GetContent (String -> a) | SetContent a String | GetCursor (Number -> a)
  type InputC = Coyoneda Input

  getContent :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g String
  getContent = liftF (inj (liftCoyoneda $ GetContent id) :: g String)

  setContent :: forall g. (Functor g, Inject (Coyoneda Input) g) => String -> Free g Unit
  setContent s = liftF (inj (liftCoyoneda $ SetContent unit s) :: g Unit)

  getCursor :: forall g. (Functor g, Inject (Coyoneda Input) g) => Free g Number
  getCursor = liftF (inj (liftCoyoneda $ GetCursor id) :: g Number)

  editorComponent :: forall eff. Component Unit InputC (Eff (dom :: DOM | eff)) Void
  editorComponent = Component { render : render, query : query }

  eval :: forall eff a. Natural Input (StateT Unit (Eff (dom :: DOM | eff)))
  eval (GetContent f  ) = lift $       f <$> effectfulGetContent
  eval (SetContent n s) = lift $ const n <$> effectfulSetContent s
  eval (GetCursor  f  ) = lift $       f <$> effectfulGetCursor

  render s = H.text "todo"

  query :: forall eff i. Free InputC i -> StateT Unit (Eff (dom :: DOM | eff)) i
  query = runFreeCM eval

  test :: Free InputC Unit
  test = do
    cursor  <- getCursor
    content <- getContent
    setContent $ content ++ (show cursor)

  foreign import effectfulGetContent :: forall eff. Eff (dom :: DOM | eff) String
  foreign import effectfulSetContent :: forall eff. String -> Eff (dom :: DOM | eff) Unit
  foreign import effectfulGetCursor  :: forall eff. Eff (dom :: DOM | eff) Number
