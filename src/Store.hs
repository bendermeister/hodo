module Store (Store (..), open, close, setTodos) where

import System.Directory (renameFile)

import Todo qualified

data Store where
  Store :: {todos :: [Todo.Todo]} -> Store
  deriving (Read, Show, Eq)

open :: FilePath -> IO Store
open path = do
  contents <- readFile path
  return $ read contents

close :: FilePath -> Store -> IO ()
close path store = do
  writeFile "hodo-temp-store" $ show store
  renameFile "hodo-temp-store" path

setTodos :: Store -> [Todo.Todo] -> Store
setTodos _ todos = Store {todos = todos}
