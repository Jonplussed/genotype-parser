module Genotype.Processor.KeepColumnNumbers
  ( process
  ) where

import Control.Applicative (many)
import Data.Attoparsec.Text (Parser)
import Genotype.Processor (Processor)

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Genotype.Types

getColumnList :: T.Text -> IO [Int]
getColumnList filename = do
  cols <- T.readFile $ T.unpack filename
  either fail return $ P.parseOnly parseColumnList cols

parseColumnList :: Parser [Int]
parseColumnList =
  many $ do
    d <- P.decimal
    P.skipSpace
    return d

keepColumns :: [Int] -> [a] -> [a]
keepColumns = go 0
  where
    go _ _ [] = []
    go _ [] remaining = remaining
    go index (c:cs) (r:rs) | index == c = r : go (succ index) cs rs
    go index cs (_:rs) = go (succ index) cs rs

process :: T.Text -> Processor
process filename genos = do
  cols <- getColumnList filename
  return . flip map genos $ \g ->
    g { geno_datums = keepColumns cols $ geno_datums g }
