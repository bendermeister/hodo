{-# LANGUAGE RecordWildCards #-}

module Cli.Add (run) where

import Context (Context, println)
import Context qualified
import Store qualified
import Todo qualified
import Util qualified
import Prelude hiding (print)

run :: Context -> [String] -> Context
run context args = run' context $ parseArgs args

data Args where
  Args :: {title :: String, prefix :: String} -> Args

type TupledArgs =
  ( Maybe Util.Flag,
    Maybe Util.Flag,
    Maybe Util.Flag,
    Maybe Util.Flag
  )

parseArgs :: [String] -> Either String Args
parseArgs = unpacker . Util.argParse [Util.strFlag "--title", Util.strFlag "-t", Util.strFlag "--prefix", Util.strFlag "-p"]
  where
    unpacker :: Either Util.ArgParseError ([Maybe Util.Flag], [String]) -> Either String Args
    unpacker (Left (Util.FlagNoValue flag)) = Left $ "ERROR: flag: '" ++ flag ++ "'" ++ "needs a value"
    unpacker (Right (flags, [])) = unpacker' $ toTuple flags
    unpacker (Right (_, args)) = Left $ "ERROR: unused args: '" ++ unwords args ++ "'"

    toTuple :: [Maybe Util.Flag] -> TupledArgs
    toTuple [a, b, c, d] = (a, b, c, d)
    toTuple _ = error "number of extractors does not match number of flags to unpack"

    unpacker' :: TupledArgs -> Either String Args
    -- title unpacking
    unpacker' (Nothing, Nothing, _, _) = Left "ERROR: no titles given"
    unpacker' (Just _, Just _, _, _) = Left "ERROR: multiple titles given"
    unpacker' (Nothing, Just title, a, b) = unpacker' (Just title, Nothing, a, b)
    -- prefix unpacking
    unpacker' (_, _, Just _, Just _) = Left "ERROR: multiple prefixes given"
    unpacker' (a, b, Nothing, Just prefix) = unpacker' (a, b, Just prefix, Nothing)
    unpacker' (a, b, Nothing, Nothing) = unpacker' (a, b, Just (Util.StrFlag ""), Nothing)
    -- finalizing
    unpacker' (Just (Util.StrFlag title), Nothing, Just (Util.StrFlag prefix), Nothing) = Right $ Args {title = title, prefix = prefix}
    unpacker' _ = undefined

argsToTodo :: Args -> Todo.Todo
argsToTodo Args {..} = Todo.Todo {prefix = prefix, title = title}

run' :: Context -> Either String Args -> Context
run' context (Left message) = println context message
run' context (Right args) =
  let todo = argsToTodo args
      store = Context.store context
      todos = Store.todos store
      store' = Store.setTodos store $ todo : todos
      context' = Context.setStore context store'
   in Context.println context' $ "Added new Todo: " ++ show todo
