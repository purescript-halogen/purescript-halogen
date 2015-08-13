module Model where

import Prelude

type TaskId = Int

type Task = { description :: String, completed :: Boolean }

type State = { tasks :: Array TaskId, nextId :: TaskId, numCompleted :: Int }

initialState :: State
initialState = { tasks: [], nextId: 1, numCompleted: 0 }
