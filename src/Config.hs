module Config (Config (..), new) where

data Config where
  Config :: {storePath :: FilePath} -> Config
  deriving (Show, Read, Eq)

new :: FilePath -> Config
new path = Config {storePath = path}
