module Driver (Mode (..), BuildOpts (..), build) where

import CodeGen (codegenModule)
import Compiler (CompiledModule (..), FnSigs, Globals, buildFnSigs, buildGlobals, compileModule)
import Control.Exception (IOException, finally, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Parser (FnDecl (..), GlobalDecl (..), GlobalKind (..), Program (..), parse, tokenize)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, replaceExtension, takeBaseName, takeDirectory, takeExtension, (</>))
import System.Process (readProcessWithExitCode)

-- どこまで処理するか（-S: コンパイルまで、-c: アセンブルまで、指定なし: リンクまで）
data Mode = AsmOnly | ObjOnly | Link deriving (Show, Eq)

data BuildOpts = BuildOpts
  { inputs :: [FilePath],
    output :: FilePath,
    mode :: Mode,
    -- 中間ファイルの出力先（-d）。Nothing ならシステムのテンポラリディレクトリに作り、終了時に削除する
    outDir :: Maybe FilePath
  }
  deriving (Show, Eq)

-- 入力ファイルの種類（拡張子で判定する）
data InputKind = Source | Asm | Obj deriving (Show, Eq)

type BuildM = ExceptT String IO

-- ソースの読み込み結果（表示用のパス, 重複判定用の正規化パス, プログラム）
data Module = Module
  { modPath :: FilePath,
    modCanonical :: FilePath,
    modProgram :: Program
  }

-- 入力ファイル群をビルドする。中間ファイルは出力ディレクトリに「basename + 拡張子」で作り、
-- -d が無ければ（成功・失敗を問わず）終了時に削除する
build :: BuildOpts -> IO (Either String ())
build opts = do
  dir <- maybe getTemporaryDirectory pure (outDir opts)
  created <- newIORef []
  let keep = isJust (outDir opts)
      cleanup = unless keep $ readIORef created >>= mapM_ removeIfExists
  (runExceptT (buildWith opts dir created) `finally` cleanup)
  where
    removeIfExists path = do
      exists <- doesFileExist path
      when exists (removeFile path)

buildWith :: BuildOpts -> FilePath -> IORef [FilePath] -> BuildM ()
buildWith opts dir created = do
  kind <- liftEither (classifyInputs (mode opts) (inputs opts))
  forM_ (inputs opts) $ \path -> do
    exists <- liftIO (doesFileExist path)
    unless exists $ throwE ("no such file: " ++ path)
  when (isJust (outDir opts)) $ io (createDirectoryIfMissing True dir)
  let intermediate path ext = dir </> replaceExtension (takeBaseName path) ext
      -- 中間ファイルは作成前に登録しておき、途中で失敗しても削除の対象にする
      newIntermediate path = do
        liftIO (modifyIORef created (path :))
        pure path
  case (mode opts, kind, inputs opts) of
    (AsmOnly, Source, [src]) -> compileSource src >>= io . writeFile (output opts)
    (ObjOnly, Source, [src]) -> do
      let asmPath = intermediate src "s"
      checkIntermediates [(src, asmPath)]
      asm <- compileSource src
      _ <- newIntermediate asmPath
      io (writeFile asmPath asm)
      assemble asmPath (output opts)
    (ObjOnly, Asm, [asmPath]) -> assemble asmPath (output opts)
    (Link, Source, srcs) -> do
      mods <- collectSources srcs
      checkLinkUnits mods
      let units = [(modPath m, intermediate (modPath m) "s", intermediate (modPath m) "o") | m <- mods]
      checkIntermediates ([(p, s) | (p, s, _) <- units] ++ [(p, o) | (p, _, o) <- units])
      objs <- forM units $ \(src, asmPath, objPath) -> do
        asm <- compileSource src
        _ <- newIntermediate asmPath
        io (writeFile asmPath asm)
        _ <- newIntermediate objPath
        assemble asmPath objPath
        pure objPath
      link objs (output opts)
    (Link, Asm, asmPaths) -> do
      let units = [(p, intermediate p "o") | p <- asmPaths]
      checkIntermediates units
      objs <- forM units $ \(asmPath, objPath) -> do
        _ <- newIntermediate objPath
        assemble asmPath objPath
        pure objPath
      link objs (output opts)
    (Link, Obj, objs) -> link objs (output opts)
    _ -> throwE "internal error: unsupported combination of mode and inputs"

-- 入力ファイルの種類と、-S/-c との組み合わせを検証する
classifyInputs :: Mode -> [FilePath] -> Either String InputKind
classifyInputs _ [] = Left "no input files"
classifyInputs m paths = do
  kinds <- mapM classify paths
  kind <- case nub kinds of
    [k] -> Right k
    _ -> Left "input files of different kinds (.sl/.s/.o) cannot be mixed"
  when (m /= Link && length paths > 1) $ Left "-S/-c accept only one input file"
  case (m, kind) of
    (AsmOnly, Source) -> Right kind
    (AsmOnly, _) -> Left "-S requires a source file (.sl)"
    (ObjOnly, Obj) -> Left "-c requires a source file (.sl) or an assembly file (.s)"
    _ -> Right kind
  where
    classify path = case takeExtension path of
      ".sl" -> Right Source
      ".s" -> Right Asm
      ".o" -> Right Obj
      _ -> Left ("unknown input file type (expected .sl/.s/.o): " ++ path)

-- 中間ファイルの名前は元のディレクトリを考慮しない（basenameのみ）ため、別ディレクトリの同名ファイルは衝突する
checkIntermediates :: [(FilePath, FilePath)] -> BuildM ()
checkIntermediates units = do
  canon <- forM units $ \(src, path) -> do
    c <- io (canonicalizePath path)
    pure (c, src)
  _ <- foldM addUnit Map.empty canon
  pure ()
  where
    addUnit seen (c, src) = case Map.lookup c seen of
      Just other | other /= src -> throwE ("intermediate file name conflict: " ++ other ++ " and " ++ src)
      _ -> pure (Map.insert c src seen)

-- 1ソースをコンパイルしてアセンブリを返す。直接 import したソースのfnのシグネチャと const/static だけを取り込む
compileSource :: FilePath -> BuildM String
compileSource path = do
  m <- loadModule path
  (sigs, globals) <- importedSigs m
  cm <- inFile path (compileModule sigs globals (modProgram m))
  pure (codegenModule (cmStatics cm) (cmFns cm) (cmMain cm))

-- ソースを読み込んでパースする（エラーにはファイル名を前置する）
loadModule :: FilePath -> BuildM Module
loadModule path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ throwE ("no such file: " ++ path)
  src <- io (readFile path)
  prog <- inFile path (tokenize src >>= parse)
  canon <- io (canonicalizePath path)
  pure (Module (normalise path) canon prog)

-- import のパスは、importを書いたソースのディレクトリからの相対パスとして解決する
resolveImports :: Module -> BuildM [FilePath]
resolveImports m =
  forM (progImports (modProgram m)) $ \imp -> do
    when (takeExtension imp /= ".sl") $
      throwE (modPath m ++ ": imported file must be a source file (.sl): " ++ imp)
    let path = normalise (takeDirectory (modPath m) </> imp)
    exists <- liftIO (doesFileExist path)
    unless exists $ throwE (modPath m ++ ": imported file not found: " ++ imp)
    pure path

-- 直接 import したソースに定義されたfnのシグネチャ（mainを除く）と const/static を集める。
-- importしたソース同士で同名の定義があればエラー（importの再帰的な取り込みは行わない）
importedSigs :: Module -> BuildM (FnSigs, Globals)
importedSigs m = do
  paths <- resolveImports m
  imported <- dedupeBy modCanonical <$> mapM loadModule paths
  perFile <- forM imported $ \im -> do
    sigs <- inFile (modPath im) (buildFnSigs (progFns (modProgram im)))
    (globals, _) <- inFile (modPath im) (buildGlobals (progGlobals (modProgram im)))
    pure (modPath im, Map.delete "main" sigs, globals)
  -- fn・const・static は同じ名前空間を共有するため、まとめて衝突を検査する
  _ <- foldM addNames Map.empty [(path, [(n, True) | n <- Map.keys sigs] ++ [(n, False) | n <- Map.keys globals]) | (path, sigs, globals) <- perFile]
  pure (Map.unions [sigs | (_, sigs, _) <- perFile], Map.unions [globals | (_, _, globals) <- perFile])
  where
    addNames owners (path, names) =
      foldM
        ( \acc (name, isFn) -> case Map.lookup name acc of
            Just (other, otherIsFn) -> throwE (modPath m ++ ": " ++ definedInBoth (isFn && otherIsFn) name other path)
            Nothing -> pure (Map.insert name (path, isFn) acc)
        )
        owners
        names

-- 同名の定義が2つのソースにあるときのメッセージ（fn同士なら従来どおり "function f ..."）
definedInBoth :: Bool -> String -> FilePath -> FilePath -> String
definedInBoth bothFns name a b =
  (if bothFns then "function " else "") ++ name ++ " is defined in both " ++ a ++ " and " ++ b

-- コンパイル〜リンク一括処理の対象となるソースを、入力から import を再帰的にたどって集める
-- （同じファイルは1回だけ。importが循環していても、共有するのはシグネチャだけなので問題ない）
collectSources :: [FilePath] -> BuildM [Module]
collectSources roots = reverse <$> foldM visit [] roots
  where
    visit acc path = do
      m <- loadModule path
      if any ((== modCanonical m) . modCanonical) acc
        then pure acc
        else do
          imports <- resolveImports m
          foldM visit (m : acc) imports

-- リンクする全ソースを通した検査: fn名・static名（どちらもシンボルになる）の重複（リンカより分かりやすい
-- メッセージにする）と main がちょうど1つあること。const はシンボルを持たないため対象外
checkLinkUnits :: [Module] -> BuildM ()
checkLinkUnits mods = do
  _ <- foldM addSymbols Map.empty mods
  let mains = [modPath m | m <- mods, "main" `elem` fnNames m]
  case mains of
    [] -> throwE "missing function: main"
    [_] -> pure ()
    _ -> throwE ("function main is defined in multiple files: " ++ unwords mains)
  where
    fnNames m = nub [name | FnDecl name _ _ _ <- progFns (modProgram m)]
    staticNames m = nub [name | GlobalDecl GStatic name _ _ <- progGlobals (modProgram m)]
    addSymbols owners m =
      foldM
        ( \acc (name, isFn) -> case Map.lookup name acc of
            Just (other, otherIsFn)
              | name == "main" && isFn && otherIsFn -> pure acc
              | otherwise -> throwE (definedInBoth (isFn && otherIsFn) name other (modPath m))
            Nothing -> pure (Map.insert name (modPath m, isFn) acc)
        )
        owners
        ([(n, True) | n <- fnNames m] ++ [(n, False) | n <- staticNames m])

assemble :: FilePath -> FilePath -> BuildM ()
assemble asmPath objPath = gcc ["-c", asmPath, "-g", "-o", objPath]

link :: [FilePath] -> FilePath -> BuildM ()
link objs outPath = gcc (objs ++ ["-o", outPath])

-- gcc を呼び出す（失敗時は gcc の標準エラー出力をエラーメッセージにする）
gcc :: [String] -> BuildM ()
gcc args = do
  (code, _out, err) <- io (readProcessWithExitCode "gcc" args "")
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwE ("gcc failed: gcc " ++ unwords args ++ "\n" ++ err)

liftEither :: Either String a -> BuildM a
liftEither = either throwE pure

-- ファイル単位の処理のエラーにファイル名を前置する
inFile :: FilePath -> Either String a -> BuildM a
inFile path = either (throwE . ((path ++ ": ") ++)) pure

-- IO例外（ファイルの読み書きなど）をエラーメッセージへ変換する
io :: IO a -> BuildM a
io action = do
  r <- liftIO (try action)
  case r of
    Left e -> throwE (show (e :: IOException))
    Right a -> pure a

dedupeBy :: (Eq k) => (a -> k) -> [a] -> [a]
dedupeBy key = go []
  where
    go _ [] = []
    go seen (x : xs)
      | key x `elem` seen = go seen xs
      | otherwise = x : go (key x : seen) xs
