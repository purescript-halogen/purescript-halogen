module Example.Interpret.Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

type Env = { githubToken :: Maybe String }

newtype App a = App (ReaderT Env Aff a)
derive instance newtypeApp ∷ Newtype (App a) _
derive newtype instance functorApp ∷ Functor App
derive newtype instance applyApp ∷ Apply App
derive newtype instance applicativeApp ∷ Applicative App
derive newtype instance bindApp ∷ Bind App
derive newtype instance monadApp ∷ Monad App

runApp :: forall a. Env -> App a -> Aff a
runApp env (App app) = runReaderT app env

class Monad m <= GithubAPI m where
  searchUser ∷ String → m String

instance githubAPI ∷ GithubAPI App where
  searchUser q = App do
    { githubToken } <- ask
    { response } <- case githubToken of
      Nothing ->
        lift $ AX.get AXResponse.string ("https://api.github.com/users/" <> q)
      Just token ->
        lift $ AX.get AXResponse.string ("https://api.github.com/users/" <> q <> "?access_token=" <> token)
    pure response

type State = { userData :: Maybe String }

data Action = FetchData

ui :: forall f i o m. GithubAPI m => H.Component HH.HTML f i o m
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  initialState :: State
  initialState = { userData: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Fetch user data" ]
      , HH.button
          [ HE.onClick \_ -> Just FetchData ]
          [ HH.text "Fetch" ]
      , HH.p_
          [ HH.text (fromMaybe "No user data" state.userData) ]
      ]

handleAction :: forall o m. GithubAPI m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  FetchData -> do
    userData <- lift $ searchUser "kRITZCREEK"
    H.put { userData: Just userData }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (runApp { githubToken: Nothing }) ui

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui' unit body
