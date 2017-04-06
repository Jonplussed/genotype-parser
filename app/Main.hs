{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<**>), (<|>), optional)
import Data.Char (toLower)
import Data.Semigroup ((<>))
import System.IO (IOMode (WriteMode), stdout, openFile)
import Genotype.Comparison (PhaseKnowledge (..))
import Genotype.Processor (Processor, preprocess)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Genotype.Parser.FastPhase as FastPhase
import qualified Genotype.Printer.Arlequin as Arlequin
import qualified Genotype.Printer.Geno as Geno
import qualified Genotype.Processor.KeepColumnNumbers as KeepColumns
import qualified Options.Applicative as O

data InputFormat
  = FastPhase
  deriving (Eq, Show)

parseInputFormat :: O.Parser InputFormat
parseInputFormat =
  O.option (O.maybeReader formats)
    (  O.long "input-format"
    <> O.metavar "NAME"
    <> O.help "Select from: fastphase"
    )
  where
    formats str = case map toLower str of
      "fastphase" -> Just FastPhase
      _ -> Nothing

data InputSource
  = STDIN
  | InputFile T.Text
  deriving (Eq, Show)

parseInputSourceFile :: O.Parser InputSource
parseInputSourceFile =
  InputFile . T.pack <$> O.strOption
    (  O.long "input-file"
    <> O.metavar "FILENAME"
    <> O.help "Input file"
    )

parseInputSource :: O.Parser InputSource
parseInputSource = parseInputSourceFile <|> pure STDIN

data OutputFormat
  = Arlequin
  | Geno
  deriving (Eq, Show)

parseOutputFormat :: O.Parser OutputFormat
parseOutputFormat =
  O.option (O.maybeReader formats)
    (  O.long "output-format"
    <> O.metavar "NAME"
    <> O.help "Select from: arlequin, geno"
    )
  where
    formats str = case map toLower str of
      "arlequin" -> Just Arlequin
      "geno" -> Just Geno
      _ -> Nothing

data OutputSink
  = STDOUT
  | OutputFile T.Text
  deriving (Eq, Show)

parseOutputSinkFile :: O.Parser OutputSink
parseOutputSinkFile =
  OutputFile . T.pack <$> O.strOption
    (  O.long "output-file"
    <> O.metavar "FILENAME"
    <> O.help "Output file"
    )

parseOutputSink :: O.Parser OutputSink
parseOutputSink = parseOutputSinkFile <|> pure STDOUT

parseFilterColumns :: O.Parser (Maybe T.Text)
parseFilterColumns =
  optional $ T.pack <$> O.strOption
    (  O.long "keep-columns"
    <> O.metavar "FILENAME"
    <> O.help "File containing list of column numbers to keep"
    )

parsePhaseKnown :: O.Parser PhaseKnowledge
parsePhaseKnown =
  O.flag Unknown Known
    (  O.long "phase-known"
    <> O.help "Differeniate between phases in comparison output"
    )

data Options = Options
  { opt_inputFormat :: InputFormat
  , opt_inputSource :: InputSource
  , opt_outputFormat :: OutputFormat
  , opt_outputSink :: OutputSink
  , opt_filterColumns :: Maybe T.Text
  , opt_phaseKnown :: PhaseKnowledge
  } deriving (Eq, Show)

parseOptions :: O.Parser Options
parseOptions =
  Options
    <$> parseInputFormat
    <*> parseInputSource
    <*> parseOutputFormat
    <*> parseOutputSink
    <*> parseFilterColumns
    <*> parsePhaseKnown

parseOptionsWithInfo :: O.ParserInfo Options
parseOptionsWithInfo =
  O.info
    (parseOptions <**> O.helper)
    (O.header "genotype-parser - parser and printer of genotype data formats")

-- TODO: ensure output file handle closes after use
main :: IO ()
main = do
  opts <- O.execParser parseOptionsWithInfo
  input <- case opt_inputSource opts of
    InputFile file -> T.readFile $ T.unpack file
    STDIN -> T.getContents
  parsed <- either fail return $ case opt_inputFormat opts of
    FastPhase -> FastPhase.runParser input
  processed <- preprocess parsed $ preprocessors opts
  sink <- case opt_outputSink opts of
    OutputFile file -> openFile (T.unpack file) WriteMode
    STDOUT -> return stdout
  case opt_outputFormat opts of
    Arlequin -> Arlequin.print (opt_phaseKnown opts) sink processed
    Geno -> Geno.print (opt_phaseKnown opts) sink processed

preprocessors :: Options -> [Processor]
preprocessors = maybe [] (\f -> [KeepColumns.process f]) . opt_filterColumns
