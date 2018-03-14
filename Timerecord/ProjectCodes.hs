module Timerecord.ProjectCodes
    (Matcher, 
    match,
    matchingFile)
where

import Control.Monad
import Text.Regex.Posix ((=~))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, dropWhile, find)
import Text.Parsec hiding (parse)

import Timerecord.Parsing (Parser, 
                           parse)


data Matcher = Matcher { matcherRegex :: String,
                         matcherCode :: String }
               deriving (Show, Eq)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

lineEnd :: Parser ()
lineEnd = void $ (try $ (char '\r' >> char '\n')) <|> char '\r' <|> char '\n'

matcherLine :: Parser Matcher
matcherLine = do 
  let separator = try $ string "->"
  
  regex <- trim <$> (anyChar `manyTill` separator)
  code <- trim <$> (anyChar `manyTill` (lineEnd <|> eof))

  return $ Matcher regex code

matchingFile :: Parser [Matcher]
matchingFile = spaces >> (matcherLine `sepBy` spaces)

match :: [Matcher] -> String -> Maybe String
match matchers string = matcherCode <$> find matching matchers
    where matching (Matcher regex _) = not $ null $ ((string =~ regex) :: [[String]])