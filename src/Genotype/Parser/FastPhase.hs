{-# LANGUAGE OverloadedStrings #-}

module Genotype.Parser.FastPhase
  ( parseGenotype
  , runParser
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import qualified Data.Char as C

import Genotype.Types

parseName :: Parser Name
parseName =
  "name" `is` do
    letters <- P.takeWhile C.isLower
    _  <- P.char '_'
    numbers <- P.takeWhile C.isDigit
    optChar <- P.option Nothing $ Just <$> P.satisfy C.isLower
    return $ Name (letters <> "_" <> numbers) optChar

parseSubpopLabel :: Parser Int
parseSubpopLabel =
  "subpop label" `is` do
    _ <- "# subpop. label: "
    C.digitToInt <$> P.digit

parseBasePair :: Parser BasePair
parseBasePair =
  "base pair" `is` do
    c <- P.satisfy C.isUpper
    case c of
      'C' -> return C
      'T' -> return T
      'A' -> return A
      'G' -> return G
      _   -> fail $ "invalid base pair: " <> [c]

parseDatum :: Parser Datum
parseDatum = "datum" `is` (parseCertain <|> parseUncertain)
  where
    parseCertain = do
      p <- parseBasePair
      return $ Certain p
    parseUncertain = do
      _ <- P.char '['
      p <- parseBasePair
      _ <- P.char ']'
      return $ Estimated p

parseDatums :: Parser [Datum]
parseDatums = "datums" `is` P.sepBy parseDatum (P.char ' ')

parseGenotype :: Parser Genotype
parseGenotype =
  "genotype" `is` do
    name <- parseName
    P.skipSpace
    label <- parseSubpopLabel
    P.skipWhile (not . P.isEndOfLine)
    P.endOfLine
    dataLine1 <- parseDatums
    P.endOfLine
    dataLine2 <- parseDatums
    return . Genotype name label $ zip dataLine1 dataLine2

parseFile :: Parser [Genotype]
parseFile =
  "genotypes file" `is` do
    P.manyTill skipAny $ P.lookAhead start
    _ <- start
    P.endOfLine
    genos <- P.sepBy parseGenotype P.endOfLine
    P.endOfLine
    _ <- end
    return genos
  where
    start = "BEGIN GENOTYPES"
    end = "END GENOTYPES"

runParser :: Text -> Either String [Genotype]
runParser = P.parseOnly parseFile

is :: String -> Parser a -> Parser a
is = flip (P.<?>)

skipAny :: Parser ()
skipAny = P.skip $ const True
