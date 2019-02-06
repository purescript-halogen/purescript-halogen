module Example.Profunctor.Ratio (Query, Ratio(..), ratioInput, ratioInputPro) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Halogen as H
import Halogen.Component.Profunctor (ProComponent(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (InputType(InputRange), StepValue(Step))

newtype Ratio = Ratio Number
derive instance newtypeRatio :: Newtype Ratio _
derive newtype instance showRatio :: Show Ratio

type Slot = H.Slot Query Ratio

data Query a = SetRatio Ratio a

data Action = UserInput Ratio

instance functorQuery :: Functor Query where
  map fn (SetRatio ratio next) = SetRatio ratio $ fn next

ratioInput :: forall m. H.Component HH.HTML Query Ratio Ratio m
ratioInput = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval
            $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
  }

render :: forall m. Ratio -> H.ComponentHTML Action () m
render ratio = HH.input
  [ HP.type_ InputRange
  , HP.value $ Number.toString $ un Ratio ratio
  , HP.min 0.0
  , HP.step $ Step 0.01
  , HP.max 1.0
  , HE.onValueInput (Just <<< UserInput <<< Ratio <<< fromMaybe 0.0 <<< Number.fromString)
  ]

handleAction :: forall m. Action -> H.HalogenM Ratio Action () Ratio m Unit
handleAction = case _ of
  UserInput ratio ->
    H.put ratio

handleQuery :: forall m a. Query a -> H.HalogenM Ratio Action () Ratio m (Maybe a)
handleQuery = case _ of
  SetRatio ratio next -> do
    H.put ratio
    pure $ Just next

ratioInputPro :: forall m. ProComponent HH.HTML Query m Ratio Ratio
ratioInputPro = ProComponent ratioInput
