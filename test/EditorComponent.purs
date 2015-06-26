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
  editorComponent = component render query
    where

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
