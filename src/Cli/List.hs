{-# LANGUAGE RecordWildCards #-}

module Cli.List
  ( run,
  )
where

import Context (Context)
import Context qualified hiding (Context)
import Data.List
import Store qualified
import Todo (Todo)
import Todo qualified hiding (Todo)
import Util qualified

data Args where
  Args :: {prefixFilter :: String} -> Args

type ArgTuple = (Maybe Util.Flag, Maybe Util.Flag)

argParse :: [String] -> Either String Args
argParse args = unpacker $ Util.argParse [Util.strFlag "--prefix", Util.strFlag "-p"] args
  where
    unpacker :: Either Util.ArgParseError ([Maybe Util.Flag], [String]) -> Either String Args
    unpacker (Left (Util.FlagNoValue flag)) = Left $ "ERROR: '" ++ flag ++ "' expected an argument"
    unpacker (Right (flags, args'))
      | not . null $ args' = Left $ "ERROR: unused arguments: '" ++ unwords args' ++ "'"
      | otherwise = Right . unpack'' . unpack' $ flags

    unpack' :: [Maybe Util.Flag] -> ArgTuple
    unpack' [a, b] = (a, b)
    unpack' _ = error "number of extractors does not match number of unpacked flags"

    unpack'' :: ArgTuple -> Args
    -- unpacking prefixFilter
    unpack'' (Nothing, Nothing) = unpack'' (Just $ Util.StrFlag "", Nothing)
    unpack'' (Nothing, Just a) = unpack'' (Just a, Nothing)
    -- finalizing
    unpack'' (Just (Util.StrFlag p), Nothing) = Args {prefixFilter = p}
    unpack'' _ = error "mismatch of expected types"

filterFromArgs :: Args -> Todo -> Bool
filterFromArgs Args {..} todo = prefixFilter `isPrefixOf` Todo.prefix todo

run :: Context -> [String] -> Context
run context args = case argParse args of
  Left err -> Context.println context err
  Right args' -> run' context args'

run' :: Context -> Args -> Context
run' context args =
  let store = Context.store context
      todos = filter (filterFromArgs args) $ Store.todos store
   in Context.println context . unlines . map show $ todos
