module HalogenC where

  import Control.Plus (Plus)
  import Data.Bifunctor (lmap)
  import Data.Tuple (Tuple(..), fst, snd)
  import Data.Maybe (Maybe(..), maybe)
  import Data.Either (Either(..))
  import Data.Functor.Coproduct (Coproduct())
  import Control.Monad.Free
  import Control.Monad.State.Class
  import Control.Monad.State.Trans
  import Control.Monad.Trans
  import Halogen.HTML
  import qualified Data.Map as M

  newtype Component s f g p = Component
    { render :: s -> HTML p (f Unit)
    , query  :: forall i. Free f i -> StateT s g i
    }

  instance functorComponent :: Functor (Component s f g) where
    (<$>) f (Component c) =
      Component { render: \s -> f `lmap` c.render s
                , query: c.query
                }

  foreign import todo :: forall a. a

  installR :: forall s f g pl pr s' f' p'. (Ord pr, Plus g) =>
    Component s f (QueryT s' f' pr p' g s) (Either pl pr) ->   -- parent
    (pr -> Tuple s' (Component s' f' g p'))               ->   -- factory
    Component (InstalledState s s' f' g pl p') (Coproduct f (ChildF pr f')) g (Either pl p')
  installR a f = todo

  installL :: forall s f g pl pr s' f' p'. (Ord pl, Plus g) =>
    Component s f (QueryT s' f' pl p' g s) (Either pl pr) ->   -- parent
    (pl -> Tuple s' (Component s' f' g p'))               ->   -- factory
    Component (InstalledState s s' f' g pr p') (Coproduct f (ChildF pl f')) g (Either pr p')
  installL a f = todo

  installAll :: forall s f g p s' f' p'. (Ord p, Plus g) =>
    Component s f (QueryT s' f' p p' g s) p               ->   -- parent
    (p -> Tuple s' (Component s' f' g p'))                ->   -- factory
    Component (InstalledState s s' f' g p p') (Coproduct f (ChildF p f')) g p'
  installAll a f = todo

  data ChildF p f i = ChildF p (f i)

  type ComponentState s f g p = Tuple s (Component s f g p)

  newtype InstalledState s s' f' g p p' = InstalledState
    { parent   :: s
    , children :: M.Map p (ComponentState s' f' g p')
    , factory  :: p -> ComponentState s' f' g p'
    }

  newtype QueryT s' f' p p' g s a = QueryT (StateT (InstalledState s s' f' g p p') g a)

  runQueryT :: forall s' f' p p' g s a. QueryT s' f' p p' g s a -> StateT (InstalledState s s' f' g p p') g a
  runQueryT (QueryT a) = a

  -- queries a particular child from the parent:
  query :: forall s s' f' p p' g. (Monad g, Ord p) => p -> (forall i. Free f' i -> QueryT s' f' p p' g s (Maybe i))
  query p q = do
    InstalledState st <- get :: QueryT s' f' p p' g s (InstalledState s s' f' g p p')
    case M.lookup p st.children of
      Nothing -> pure Nothing
      Just (Tuple s comp@(Component c)) ->
        QueryT $ do
          Tuple i s' <- lift $ runStateT (c.query q) s
          put $ InstalledState st { children = M.insert p (Tuple s' comp) st.children }
          pure (Just i)

  -- lifts an effect into the QueryT monad:
  effect :: forall s' f' p p' g s a. (Monad g) => g a -> QueryT s' f' p p' g s a
  effect ga = QueryT (lift ga)

  instance functorQueryT :: (Monad g) => Functor (QueryT s' f' p p' g s) where
    (<$>) f (QueryT a) = QueryT (f <$> a)

  instance applyQueryT :: (Monad g) => Apply (QueryT s' f' p p' g s) where
    (<*>) (QueryT f) (QueryT a) = QueryT (f <*> a)

  instance applicativeQueryT :: (Monad g) => Applicative (QueryT s' f' p p' g s) where
    pure a = QueryT (pure a)

  instance bindQueryT :: (Monad g) => Bind (QueryT s' f' p p' g s) where
    (>>=) (QueryT a) f = QueryT $ a >>= runQueryT <<< f

  instance monadQueryT :: (Monad g) => Monad (QueryT s' f' p p' g s)

  instance monadStateQueryT :: (Monad g) => MonadState (InstalledState s s' f' g p p') (QueryT s' f' p p' g s) where
    state f = QueryT (state f)

  instance functorChildF :: (Functor f) => Functor (ChildF p f) where
    (<$>) f (ChildF p fi) = ChildF p (f <$> fi)

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
