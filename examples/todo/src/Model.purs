module Model where

type TaskId = Int

type Task =
  { description :: String
  , completed :: Boolean
  }

initialTask :: Task
initialTask =
  { description: ""
  , completed: false
  }

type List =
  { tasks :: Array TaskId
  , nextId :: TaskId
  , numCompleted :: Int
  }

initialList :: List
initialList =
  { tasks: []
  , nextId: 1
  , numCompleted: 0
  }
