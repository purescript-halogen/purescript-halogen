module Test.Component.ForkTest where

import Prelude

import Data.Foldable (traverse_)
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Halogen as H
import Halogen.Subscription as HS
import Test.Assert (assertEqual)
import Test.TestDriver as TD

type State = Maybe H.ForkId

data Query a
  = StartFork a
  | KillFork a

newtype Message = Message String

derive newtype instance eqMessage :: Eq Message
derive newtype instance showMessage :: Show Message

component :: H.Component Query Unit Message Aff
component =
  H.mkComponent
    { initialState: const Nothing
    , render: TD.render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

handleQuery :: forall a. Query a -> H.HalogenM State (Query Unit) () Message Aff (Maybe a)
handleQuery = case _ of
  StartFork a -> do
    let
      loop = do
        H.liftAff $ Aff.delay (Aff.Milliseconds 100.0)
        H.raise (Message "Progress")
        loop
    H.raise (Message "Starting")
    fid <- H.fork loop
    H.put (Just fid)
    pure (Just a)
  KillFork a -> do
    H.get >>= traverse_ \fid -> do
      H.put Nothing
      H.kill fid
    H.raise (Message "Killed")
    pure (Just a)

testForkKill :: Aff Unit
testForkKill = do
  io <- TD.runUI component unit

  logRef <- H.liftEffect $ Ref.new L.Nil

  _ <- H.liftEffect $ HS.subscribe io.messages \msg -> do
    H.liftEffect $ Ref.modify_ (msg : _) logRef
    pure Nothing

  _ <- io.query (H.mkTell StartFork)
  Aff.delay (Aff.Milliseconds 350.0)
  _ <- io.query (H.mkTell KillFork)

  -- TODO: revisit this: why do we need to wait to receive `raise`d messages
  -- from the component, if the `raise` occurs after any bind?
  Aff.delay (Aff.Milliseconds 15.0)

  logOut <- L.reverse <$> H.liftEffect (Ref.read logRef)

  H.liftEffect $ assertEqual
    { expected:
        Message <$>
          "Starting"
            : "Progress"
            : "Progress"
            : "Progress"
            : "Killed"
            : L.Nil
    , actual: logOut
    }

testFinalize :: Aff Unit
testFinalize = do
  io <- TD.runUI component unit

  logRef <- H.liftEffect $ Ref.new L.Nil

  _ <- H.liftEffect $ HS.subscribe io.messages \msg -> do
    H.liftEffect $ Ref.modify_ (msg : _) logRef
    pure Nothing

  _ <- io.query (H.mkTell StartFork)
  io.dispose
  Aff.delay (Aff.Milliseconds 350.0)

  logOut <- L.reverse <$> H.liftEffect (Ref.read logRef)

  H.liftEffect $ assertEqual
    { expected: Message <$> "Starting" : L.Nil
    , actual: logOut
    }
