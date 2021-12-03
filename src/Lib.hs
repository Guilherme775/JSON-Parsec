module Lib
    ( someFunc
    ) where

import Text.Parsec
import Control.Monad
import qualified Data.Functor.Identity
import Data.List
import Data.Char

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving(Show)

nullParser :: Parsec String () JsonValue
nullParser = JsonNull <$ string "null"

boolParser :: Parsec String () JsonValue
boolParser = chooseBool <$> choice [string "true", string "false"]
                where
                    chooseBool "true" = JsonBool True
                    chooseBool "false" = JsonBool False
                    chooseBool _ = undefined

numberParser :: Parsec String () JsonValue
numberParser = JsonNumber <$> (read <$> many1 digit)

stringParser :: Parsec String () JsonValue
stringParser = JsonString <$> between (char '"') (char '"') (many $ noneOf ['"'])

parser :: ParsecT String () Data.Functor.Identity.Identity JsonValue
parser = try nullParser <|> try boolParser <|> try numberParser <|>
             try stringParser <|> try arrayParser <|> try objectParser

arrayParser :: Parsec String () JsonValue
arrayParser = JsonArray <$> between (char '[') (char ']') (try (spaces *> elements <* spaces) <|> pure [])
                where
                    elements =
                        (spaces *> parser <* spaces) `sepBy1` (spaces *> char ',' <* spaces)

objectParser :: Parsec String () JsonValue
objectParser = JsonObject <$> between (char '{') (char '}') (spaces *> elements `sepBy1` (spaces *> char ',' <* spaces) <* spaces)
                    where
                        elements = (\key _ value -> (key, value)) <$>
                            between (char '"') (char '"') (many $ noneOf ['"']) <*>
                            (spaces *> char ':' <* spaces) <*>
                            (spaces *> parser <* spaces)

eval :: String -> Either ParseError JsonValue
eval = parse parser "unknown"

parseFile :: FilePath -> IO (Either ParseError JsonValue)
parseFile file = do
    input <- readFile file
    return $ parse parser "unknown" input

someFunc :: IO ()
someFunc = putStrLn "someFunc"
