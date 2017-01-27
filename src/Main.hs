{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Options.Applicative
import System.Exit
import Text.Comma
import Text.Tabl
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Options


-- | Parse the visual decoration settings.
parseDecor :: A.Parser Decoration -- ^ parser
parseDecor = A.string "all"     *> pure DecorAll
         <|> A.string "none"    *> pure DecorNone
         <|> A.string "inner"   *> pure DecorInner
         <|> A.string "outer"   *> pure DecorOuter
         <|> A.string "only("   *> (DecorOnly   <$> decimal) <* A.char ')'
         <|> A.string "except(" *> (DecorExcept <$> decimal) <* A.char ')'
         <|> A.string "union("  *> (DecorUnion  <$> recurse) <* A.char ')'
         <|> A.string "isect("  *> (DecorIsect  <$> recurse) <* A.char ')'
         A.<?> "invalid decoration"
  where
    recurse = A.sepBy parseDecor (A.char ',')
    decimal = A.sepBy A.decimal  (A.char ',')

-- | Parse the column alignment settings.
parseAligns :: A.Parser [Alignment] -- ^ parser
parseAligns = (A.endOfInput *> pure []) <|> A.sepBy1 align (A.char ',')
  where
    align = A.string "left"   *> pure AlignLeft
        <|> A.string "centre" *> pure AlignCentre
        <|> A.string "right"  *> pure AlignRight
        A.<?> "invalid alignment"

-- | Create a table based on the file content and command-line options.
tablize :: Options              -- ^ command-line options
        -> [[T.Text]]           -- ^ CSV table
        -> Either String T.Text -- ^ error | table
tablize options cells = do
  hdecor <- A.parseOnly parseDecor  (optHorizontal options)
  vdecor <- A.parseOnly parseDecor  (optVertical options)
  aligns <- A.parseOnly parseAligns (optAlignment options)
  return $ tabl EnvAscii hdecor vdecor aligns cells

-- | Read the input file text. In case that no file was specified, the
-- standard input is used.
getInput :: Options   -- ^ command-line options
         -> IO T.Text -- ^ input
getInput options = case optFile options of
  Just file -> T.readFile file
  Nothing   -> T.getContents

-- | Pretty-printing of CSV files.
main :: IO ()
main = do
  options <- execParser Options.parser
  input   <- getInput options
  case comma input >>= tablize options of
    Left err  -> T.putStrLn ("ERROR: " <> T.pack err) >> exitFailure
    Right res -> T.putStrLn res                       >> exitSuccess
