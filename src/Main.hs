module Main where

import Cli.Add qualified
import Cli.List qualified
import Context (Context)
import Context qualified
import System.Environment (getArgs)

run :: Context -> [String] -> Context
run context ("add" : args) = Cli.Add.run context args
run context ("list": args) = Cli.List.run context args
run context ("ls": args) = Cli.List.run context args
run context (cmd : _) = Context.println context $ "ERROR: not a valid command: " ++ cmd
run context [] = Context.println context "ERROR: you need to use a command"

main :: IO ()
main = do
  context <- Context.open
  args <- getArgs
  let context' = run context args
  Context.close context'
