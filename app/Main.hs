module Main where

import Driver (BuildOpts (..), Mode (..), build)
import Options.Applicative
import System.Exit (exitFailure)

optionsParser :: Parser BuildOpts
optionsParser =
  BuildOpts
    <$> some (argument str (metavar "FILES..." <> help "Input files (.sl / .s / .o, all of the same kind)"))
    <*> option str (short 'o' <> metavar "OUTPUT" <> help "Output file name")
    <*> modeParser
    <*> optional (option str (short 'd' <> metavar "OUTDIR" <> help "Directory for intermediate files (kept after the build; default: system temporary directory, removed after the build)"))

-- -S と -c は排他。どちらも無ければリンクまで行う
modeParser :: Parser Mode
modeParser =
  flag' AsmOnly (short 'S' <> help "Compile only; output an assembly file (single input)")
    <|> flag' ObjOnly (short 'c' <> help "Compile and assemble; output an object file (single input)")
    <|> pure Link

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "A simple programming language compiler written in Haskell"
            <> header "simplang-haskell - x86-64 native code generator"
        )
  result <- build opts
  case result of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right () -> pure ()
