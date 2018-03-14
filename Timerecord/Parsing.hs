module Timerecord.Parsing (Parser,
                           parseNumber,
                           parse,
                           parseHM,
                           parseMinutes)
where 

import Text.Parsec hiding (parse)
import Text.Parsec.Error
import qualified Text.Parsec as Parsec

import Control.Applicative hiding ((<|>), many)
import Control.Monad (when)

type Parser = Parsec.Parsec String ()

parseNumber :: Parser Int
parseNumber = read <$> many1 digit
-- parse p = either (const Nothing) Just . Parsec.parse p ""

parse :: String -> Parser a -> String -> Either String a
parse what p str = either msg Right $ Parsec.parse p "" str
    where msg s = Left $ "Can't parse \"" ++ str ++ "\" as " ++ what ++ ": " 
                  ++ (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ errorMessages s)

parseHM = do (h, m) <- ((,) <$> (parseNumber <* char ':') <*> parseNumber)
             when (m > 59) $ parserFail "In h:m m cant be > 59 "
             return $ m + h * 60

parseMinutes :: String -> Either String Int
parseMinutes = parse "minutes" $ ((try $ parseHM) <|> (parseNumber)) <* eof
