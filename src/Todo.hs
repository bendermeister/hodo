module Todo (Todo (..)) where

data Todo where
  Todo :: {prefix :: String, title :: String} -> Todo
  deriving (Read, Show, Eq)
