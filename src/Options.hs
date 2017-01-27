module Options
( Options(..)
, parser
) where

import Data.Char
import Options.Applicative
import qualified Data.Text as T

-- | Command-line options.
data Options = Options
  { optFile       :: Maybe String
  , optHorizontal :: T.Text
  , optVertical   :: T.Text
  , optAlignment  :: T.Text }

-- | Relevant file extensions options.
parseFile :: Parser (Maybe String) -- ^ parser
parseFile = optional $ strArgument (metavar "FILE")

-- | Helper funciton to remove all whitespace from a string.
noSpace :: String -- ^ old string
        -> String -- ^ new string
noSpace = filter (not . isSpace)

-- | Parse horizontal decoration.
parseHorizontal :: Parser T.Text -- ^ parser
parseHorizontal = fmap (T.pack . noSpace) $ strOption
   $ short   'x'
  <> long    "horizontal"
  <> value   "union(only(1),outer)"
  <> metavar "HDECOR"
  <> help    "Horizontal decoration"
  <> showDefault

-- | Relevant file extensions options.
parseVertical :: Parser T.Text -- ^ parser
parseVertical = fmap (T.pack . noSpace) $ strOption
   $ short   'y'
  <> long    "vertical"
  <> value   "all"
  <> metavar "VDECOR"
  <> help    "Vertical decoration"
  <> showDefault

-- | Relevant file extensions options.
parseAlignment :: Parser T.Text -- ^ parser
parseAlignment = fmap (T.pack . noSpace) $ strOption
   $ short   'a'
  <> long    "alignment"
  <> value   ""
  <> metavar "ALIGNS"
  <> help    "Comma-separated list of column alignments. E.g.: left,right"
  <> showDefault

-- | Command-line user interface.
optionsParser :: Parser Options -- ^ parser
optionsParser = Options
  <$> parseFile
  <*> parseHorizontal
  <*> parseVertical
  <*> parseAlignment

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) (header top <> fullDesc)
  where top = "tablize - CSV file pretty-printer"
