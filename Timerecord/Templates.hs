{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Timerecord.Templates
    (TemplateEntry,
     TemplateElement,
     templateParser,
     templateHelp,
     teElement,
     teString,
     teDescription,
     teKey)
where

import Text.Parsec
import Timerecord.Parsing
import Control.Applicative hiding ((<|>), many)

class TemplateEntry e where
    teString :: String -> e

class (TemplateEntry e) => TemplateElement template e | template -> e where
    teElement :: template -> e
    teDescription :: template -> String
    teKey :: template -> String

templateParser :: (TemplateElement t e) => [t] -> Parser [e]
templateParser teTemplates = many $ choice $ keys ++ [openBracket, rawString]
    where keys = map (\t -> do try $ string $ teKey t
                               return $ teElement t) 
                 teTemplates
          openBracket = string "{" >> (return $ teString "{")
          rawString = teString <$> (many1 $ noneOf "{")

templateHelp :: (TemplateElement t e) => [t] -> [String]
templateHelp teTemplates = 
    (["Format file can contain next keys:"]
     ++ map ((++) <$> (" " ++) . teKey <*> (" - " ++) . teDescription) teTemplates
     ++ ["Any other text will be output as is."])
