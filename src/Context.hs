{-# LANGUAGE RecordWildCards #-}

module Context (Context (..), setStore, open, close, print, println) where

import Config (Config)
import Config qualified
import Data.List
import Store (Store)
import Store qualified
import System.Environment (getEnvironment)
import Prelude hiding (print)

data Context where
  Context :: {stdin :: String, stdout :: String, store :: Store, config :: Config} -> Context
  deriving ()

setStore :: Context -> Store -> Context
setStore Context {..} store' = Context {store = store', config = config, stdin = stdin, stdout = stdout}

open :: IO Context
open = do
  env <- getEnvironment
  let storePath = findStorePath env
  store <- Store.open storePath
  stdin <- getContents
  let config = Config.new storePath
  return $
    Context
      { store = store,
        config = config,
        stdin = stdin,
        stdout = ""
      }
  where
    unpack (Just a) = a
    unpack Nothing = error "tried to unwrap a nothing"

    findStorePath :: [(String, String)] -> String
    findStorePath = unpack . fmap snd . find (\(key, _) -> key == "HODO_STORE_PATH")

print :: Context -> String -> Context
print context p =
  Context
    { store = store context,
      config = config context,
      stdin = stdin context,
      stdout = stdout context ++ p
    }

println :: Context -> String -> Context
println context p = print context $ p ++ "\n"

close :: Context -> IO ()
close context = do
  Store.close (Config.storePath $ config context) $ store context
  putStr $ stdout context
