module Driver (Mode (..), BuildOpts (..), CodegenInput (..), build) where

import CodeGen (codegen)
import Compiler (CompiledModule (..), FnSigs, Globals, Instr, StaticData, buildFnSigs, buildGlobals, compile, publicNames)
import Control.Exception (IOException, finally, try)
import Control.Monad (foldM, forM, forM_, unless, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import Parser (FnDecl (..), GlobalDecl (..), GlobalKind (..), Program (..), Type, Visibility (..), parse, tokenize)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, replaceExtension, takeBaseName, takeDirectory, takeExtension, (</>))
import System.Process (readProcessWithExitCode)
import Text.Pretty.Simple (pShowNoColor)

-- どこまで処理するか（-i: codegen の入力まで、-S: コンパイルまで、-c: アセンブルまで、指定なし: リンクまで）
data Mode = IROnly | AsmOnly | ObjOnly | Link deriving (Show, Eq)

-- codegen の入力。-i と中間ファイル .sli はこれを整形して書き出したもの（レコードにしてフィールド名も出力する）
data CodegenInput = CodegenInput
  { -- .globl で公開するシンボル
    public :: [String],
    -- static の初期値
    statics :: [StaticData],
    -- main以外のfnの (名前, 戻り値の型, 命令列)
    fns :: [(String, Type, [Instr])],
    -- mainがあれば (mainの戻り値型, mainの命令列)
    mainPart :: Maybe (Type, [Instr])
  }
  deriving (Show)

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
      -- 入力群の中間ファイル（拡張子 exts）の名前が衝突しないかを、作成を始める前にまとめて検査する
      checkExts exts paths = checkIntermediates [(p, intermediate p e) | e <- exts, p <- paths]
      -- 中間ファイルは作成前に登録しておき、途中で失敗しても削除の対象にする
      createIntermediate :: FilePath -> (FilePath -> BuildM ()) -> BuildM FilePath
      createIntermediate path make = do
        liftIO (modifyIORef created (path :))
        make path
        pure path
      -- .sl → 中間の .s（コンパイルに失敗したら .s は作らない）。withIR なら codegen の入力を中間の .sli にも書く
      toAsm withIR src = do
        ir <- compileIR src
        when withIR $ () <$ createIntermediate (intermediate src "sli") (io . (`writeFile` showIR ir))
        createIntermediate (intermediate src "s") (io . (`writeFile` genAsm ir))
      -- .s → 中間の .o
      toObj asmPath = createIntermediate (intermediate asmPath "o") (assemble asmPath)
  case (mode opts, kind, inputs opts) of
    (IROnly, Source, [src]) -> compileIR src >>= io . writeFile (output opts) . showIR
    (AsmOnly, Source, [src]) -> compileIR src >>= io . writeFile (output opts) . genAsm
    (ObjOnly, Source, [src]) -> do
      checkExts ["s"] [src]
      asmPath <- toAsm False src
      assemble asmPath (output opts)
    (ObjOnly, Asm, [asmPath]) -> assemble asmPath (output opts)
    (Link, Source, srcs) -> do
      mods <- collectSources srcs
      checkLinkUnits mods
      let paths = map modPath mods
      checkExts ["sli", "s", "o"] paths
      objs <- mapM (toAsm True >=> toObj) paths
      link objs (output opts)
    (Link, Asm, asmPaths) -> do
      checkExts ["o"] asmPaths
      objs <- mapM toObj asmPaths
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
  when (m /= Link && length paths > 1) $ Left "-S/-c/-i accept only one input file"
  case (m, kind) of
    (IROnly, Source) -> Right kind
    (IROnly, _) -> Left "-i requires a source file (.sl)"
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

-- 1ソースをコンパイルして codegen の入力を返す。直接 import したソースの pub の fn のシグネチャと const/static だけを取り込む
compileIR :: FilePath -> BuildM CodegenInput
compileIR path = do
  m <- loadModule path
  (sigs, globals) <- importedSigs m
  cm <- inFile path (compile sigs globals (modProgram m))
  pure (CodegenInput (cmPublic cm) (cmStatics cm) (cmFns cm) (cmMain cm))

-- codegen の入力からアセンブリを作る
genAsm :: CodegenInput -> String
genAsm ir = codegen (public ir) (statics ir) (fns ir) (mainPart ir)

-- .sli（-i の出力）の中身：codegen の入力を pretty-simple で整形したもの（pPrint と同じ整形で、色のエスケープは付けない）
showIR :: CodegenInput -> String
showIR ir = TL.unpack (pShowNoColor ir) ++ "\n"

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

-- 直接 import したソースに定義された pub の fn のシグネチャ（mainを除く）と const/static を集める。
-- 非公開の定義は取り込まない（評価はソース全体で行う。pub の const が非公開の const を参照していてもよい）。
-- importしたソース同士で同名の pub の定義があればエラー（importの再帰的な取り込みは行わない）
importedSigs :: Module -> BuildM (FnSigs, Globals)
importedSigs m = do
  paths <- resolveImports m
  imported <- dedupeBy modCanonical <$> mapM loadModule paths
  perFile <- forM imported $ \im -> do
    sigs <- inFile (modPath im) (buildFnSigs (progFns (modProgram im)))
    (globals, _) <- inFile (modPath im) (buildGlobals (progGlobals (modProgram im)))
    let pubNames = Map.fromList [(name, ()) | name <- publicNames (modProgram im)]
    pure (modPath im, Map.delete "main" (Map.intersection sigs pubNames), Map.intersection globals pubNames)
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

-- リンクする全ソースを通した検査: pub の fn名・static名（どちらも .globl のシンボルになる）の重複（リンカより
-- 分かりやすいメッセージにする）と main がちょうど1つあること。非公開の fn・static はファイルローカルなシンボルで、
-- const はシンボルを持たないため対象外
checkLinkUnits :: [Module] -> BuildM ()
checkLinkUnits mods = do
  _ <- foldM addSymbols Map.empty mods
  let mains = [modPath m | m <- mods, "main" `elem` fnNames m]
  case mains of
    [] -> throwE "missing function: main"
    [_] -> pure ()
    _ -> throwE ("function main is defined in multiple files: " ++ unwords mains)
  where
    fnNames m = nub [name | FnDecl _ name _ _ _ <- progFns (modProgram m)]
    -- main は pub の有無によらず常に .globl
    pubFnNames m = nub [name | FnDecl vis name _ _ _ <- progFns (modProgram m), vis == Public || name == "main"]
    pubStaticNames m = nub [name | GlobalDecl Public GStatic name _ _ <- progGlobals (modProgram m)]
    addSymbols owners m =
      foldM
        ( \acc (name, isFn) -> case Map.lookup name acc of
            Just (other, otherIsFn)
              | name == "main" && isFn && otherIsFn -> pure acc
              | otherwise -> throwE (definedInBoth (isFn && otherIsFn) name other (modPath m))
            Nothing -> pure (Map.insert name (modPath m, isFn) acc)
        )
        owners
        ([(n, True) | n <- pubFnNames m] ++ [(n, False) | n <- pubStaticNames m])

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
