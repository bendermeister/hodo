module Config
  ( Config (..),
  )
where

data Config where
  Config :: {storePath :: FilePath} -> Config
  deriving (Eq, Show, Read)
