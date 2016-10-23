module Control.Monad.Fork where

import Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Trans (lift)

class Monad m ⇐ MonadFork e m where
  fork ∷ ∀ a. m a → m (e -> m Boolean)

instance monadForkAff ∷ MonadFork Error (Aff.Aff eff) where
  fork aff = do
    ac ← Aff.forkAff aff
    pure $ \reason → Aff.cancel ac reason

instance monadForkReaderT ∷ MonadFork e m ⇒ MonadFork e (ReaderT r m) where
  fork (ReaderT ma) =
    ReaderT \r → map lift <$> fork (ma r)
