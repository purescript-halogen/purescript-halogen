module Example.Components.Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const(), getConst)
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe)
import Data.Void (Void(), absurd)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Example.Components.List
import Example.Components.Ticker

type Query = Const Void

type State = { count :: Int }

initialState :: State
initialState = { count: 0 }

newtype ListSlot = ListSlot String

derive instance genericListSlot :: Generic ListSlot
instance eqListSlot :: Eq ListSlot where eq = gEq
instance ordListSlot :: Ord ListSlot where compare = gCompare

type QueryP = Coproduct Query (ChildF ListSlot ListQueryP)
type StateP g = InstalledState State (ListStateP g) Query ListQueryP g ListSlot

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent' render eval peek
  where

  render :: State -> ParentHTML (ListStateP g) Query ListQueryP g ListSlot
  render st =
    H.div_
      [ H.h1_
          [ H.text "List A" ]
      , H.slot (ListSlot "A") \_ ->
          { component: listComponent, initialState: installedState initialList }
      , H.hr []
      , H.h1_
          [ H.text "List B" ]
      , H.slot (ListSlot "B") \_ ->
          { component: listComponent, initialState: installedState initialList }
      , H.hr []
      , H.p_
          [ H.text $ "Total incremented count: " ++ show st.count ]
      ]

  eval :: Natural Query (ParentDSL State (ListStateP g) Query ListQueryP g ListSlot)
  eval = absurd <<< getConst

  peek :: forall a. ChildF ListSlot ListQueryP a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peek (ChildF _ q) = coproduct peekList peekTicker q

  peekList :: forall a. ListQuery a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekList _ =
    -- we're not actually interested in peeking on the list.
    -- instead of defining a function like this, an alternative would be to use
    -- `(const (pure unit))` in place of `peekList` in the `coproduct` function
    pure unit

  peekTicker :: forall a. ChildF TickSlot TickQuery a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekTicker (ChildF _ (Tick _)) = modify (\st -> { count: st.count + 1 })
  peekTicker _ = pure unit

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  onLoad $ appendToBody app.node
