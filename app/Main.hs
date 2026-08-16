module Main where

import CodeGen (codegen)
import Compiler (compile, parse, tokenize)
import Options.Applicative
import System.Directory (getTemporaryDirectory, renameFile)
import System.Exit (exitFailure)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (callProcess)

data Options = Options
  { sourceFile :: FilePath
  , outputFile :: FilePath
  , asmFile    :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "FILE" <> help "Source file to compile")
    <*> option str (long "output" <> short 'o' <> value "out" <> metavar "NAME" <> help "Output file name (default: out)")
    <*> optional (option str (short 'S' <> metavar "FILE" <> help "Save assembly source to FILE"))

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Compile an arithmetic expression to a native binary"
            <> header "hs006 - x86-64 native code generator"
        )
  source <- readFile (sourceFile opts)
  case tokenize source >>= parse >>= compile of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right (fns, finalType, instrs) -> do
      let asm = codegen fns finalType instrs
      tmpDir <- getTemporaryDirectory
      (tmpPath, tmpHandle) <- openTempFile tmpDir "hs006.s"
      hPutStr tmpHandle asm
      hClose tmpHandle
      asmPath <- case asmFile opts of
        Just path -> renameFile tmpPath path >> return path
        Nothing   -> return tmpPath
      callProcess "gcc" [asmPath, "-o", outputFile opts]
