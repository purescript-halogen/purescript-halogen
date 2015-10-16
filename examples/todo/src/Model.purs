module Model where

import Prelude

type TaskId = Int

type Task = { description :: String, completed :: Boolean }

type List = { tasks :: Array TaskId, nextId :: TaskId, numCompleted :: Int }

initialList :: List
initialList = { tasks: [], nextId: 1, numCompleted: 0 }

initialTask :: Task
initialTask = { description: "", completed: false }
