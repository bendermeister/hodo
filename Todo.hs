module Todo () where

import Config (Config)
import Config qualified

data Todo where
  Todo :: {title :: String} -> Todo
  deriving (Show, Read, Eq)

fetch :: Config -> IO [Todo]
fetch config = do
  contents <- readFile $ Config.storePath config
  return $ map read . filter (not . null) $ lines contents

store :: Config -> Todo -> IO ()
store config todo = do
  appendFile path $ show todo ++ "\n"
  where
    path = Config.storePath config
