module Util (argParse, Flag(..), ArgParseError(..), FlagExtractor, AppliedFlagExtractor, strFlag, boolFlag) where

data Flag = BoolFlag Bool | StrFlag String

data ArgParseError where
  FlagNoValue :: String -> ArgParseError

strFlag :: FlagExtractor
strFlag flag args = strFlag' flag args []

type FlagExtractor = String -> [String] -> Either ArgParseError (Maybe Flag, [String])

type AppliedFlagExtractor = [String] -> Either ArgParseError (Maybe Flag, [String])

strFlag' :: String -> [String] -> [String] -> Either ArgParseError (Maybe Flag, [String])
strFlag' _ [] acc = Right (Nothing, acc)
strFlag' flag [flag'] acc
  | flag == flag' = Left $ FlagNoValue flag
  | otherwise = Right (Nothing, acc ++ [flag'])
strFlag' flag (flag' : value : args) acc
  | flag == flag' = Right (Just $ StrFlag value, acc ++ args)
  | otherwise = strFlag' flag (value : args) (acc ++ [flag'])

boolFlag :: FlagExtractor
boolFlag flag args = boolFlag' flag args []

boolFlag' :: String -> [String] -> [String] -> Either ArgParseError (Maybe Flag, [String])
boolFlag' _ [] acc = Right (Nothing, acc)
boolFlag' flag (flag' : args) acc
  | flag == flag' = Right (Just $ BoolFlag True, acc ++ args)
  | otherwise = boolFlag' flag args $ acc ++ [flag']

argParse :: [AppliedFlagExtractor] -> [String] -> Either ArgParseError ([Maybe Flag], [String])
argParse extractors args = argParse' extractors args []

argParse' :: [AppliedFlagExtractor] -> [String] -> [Maybe Flag] -> Either ArgParseError ([Maybe Flag], [String])
argParse' [] args acc = Right (acc, args)
argParse' (extractor : extractors) args acc = case extractor args of
  (Left err) -> Left err
  (Right (flag, args')) -> argParse' extractors args' $ acc ++ [flag]
