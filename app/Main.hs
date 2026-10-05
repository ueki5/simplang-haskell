module Main where

import CodeGen (codegen)
import Compiler (compile)
import Options.Applicative
import Parser (parse, tokenize)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.Process (callProcess)

data Options = Options
  { sourceFile :: FilePath,
    outputFile :: Maybe FilePath,
    asmFile :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "FILE" <> help "Source file to compile")
    <*> optional (option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output file name (default: FILE without extension)"))
    <*> optional (option str (long "assembly" <> short 'S' <> metavar "FILE" <> help "Save assembly source to FILE"))

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
  source <- readFile (sourceFile opts)
  let outDir = "output"
      baseName = takeBaseName (sourceFile opts)
      outPath = maybe (outDir </> baseName) id (outputFile opts)
      asmPath = maybe (outDir </> (baseName ++ ".s")) id (asmFile opts)
  case tokenize source >>= parse >>= compile of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right (fns, finalType, instrs) -> do
      let asm = codegen fns finalType instrs
      _ <- createDirectoryIfMissing True outDir
      writeFile asmPath asm
      callProcess "gcc" [asmPath, "-g", "-o", outPath]
