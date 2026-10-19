module Compiler (Instr (..), FnSigs, Loc (..), Globals, StaticInit (..), StaticData, CompiledModule (..), compile, compileModule, buildFnSigs, buildGlobals, publicNames, run) where

import Control.Monad (foldM, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word32, Word64)
import Parser (Expr (..), FnDecl (..), GlobalDecl (..), GlobalKind (..), Program (..), Signedness (..), Stmt (..), Type (..), Visibility (..), Width (..), intRepr)

-- -- Debug Printサンプル（pTraceShow: 純粋関数内, pTraceShowM: モナド内）
-- import Debug.Pretty.Simple (pTraceShow, pTraceShowM)

-- -- プリティ印刷が不要な場合
-- -- trace: 純粋関数内(既存の第一引数を表示、第二引数を返却)
-- -- print/putStrLn: IOモナド内
-- import Debug.Trace (trace)

data Instr
  = Push Int
  | IAdd Width
  | ISub Width
  | IMul Width
  | IDiv Width
  | -- 符号なし除算（u32/u64 用。加減乗は符号の有無で下位ビットが変わらないため専用命令は不要）
    IDivU Width
  | INeg Width
  | Load Width Int
  | Store Width Int
  | ICmpEq
  | ICmpNe
  | ICmpLt
  | ICmpLe
  | ICmpGt
  | ICmpGe
  | -- 符号なし大小比較（u32 はゼロ拡張済みのため、幅によらず64bitの符号なし比較でよい）
    ICmpLtU
  | ICmpLeU
  | ICmpGtU
  | ICmpGeU
  | INot
  | -- スタック先頭をpopしてゼロ判定し、真（非ゼロ）ならフォールスルー、偽（ゼロ）ならジャンプする
    JmpIfZero String
  | -- 無条件ジャンプ
    Jmp String
  | -- ジャンプ先ラベルの定義
    Label String
  | -- 下位32bitを符号拡張して64bitへ戻す（as i64/as i32 で共通の命令。
    -- i32→i64 の拡大は被演算子が既に正規化済みi32であることが前提だが、
    -- リテラル直渡し等で正規化されていない値が漏れないよう、両方向とも同じ命令で強制的に正規化する）
    ISext32
  | -- 下位32bitをゼロ拡張して64bitへ戻す（u32 の正規化。as u32 や u32 の演算・読み出しの後に使う）
    IZext32
  | -- ローカル変数の%rbp相対実効アドレスを計算しpushする（&lvalue 用）
    LoadAddr Int
  | -- 操作スタック先頭のアドレスをpopし、指定幅でその先の値をロードして
    -- （64bit正規化済みで）push する（*式 用）
    LoadInd Width
  | -- 関数の第N引数レジスタ（SysV AMD64の整数引数レジスタ、0始まり最大6個）の値を
    -- 指定オフセットへストアする（関数プロローグ直後、パラメータをローカル変数と同じ
    -- スタックスロットへスピルするために使う。操作スタックには触れない）
    StoreArg Int Width Int
  | -- 操作スタック先頭の配列添字を（popせずに）検査し、符号なしで要素数（Int）以上なら
    -- 範囲外エラー（.Lindex_error）へジャンプする
    IBoundsCheck Int
  | -- 値、アドレスの順にpopし、指定幅でアドレスの先へストアする（配列要素への代入 用）
    StoreInd Width
  | -- 関数呼び出し: 操作スタックに積まれた引数（左から順、個数はIntで指定）を
    -- 引数レジスタへpopしてcallし、戻り値を%raxからpushして戻す
    ICall String Int
  | -- 操作スタック先頭を捨てる（式文の値の破棄 用）
    IPop
  | -- static の値を指定幅で（64bit正規化済みで）読み出して push する（RIP相対アドレッシング）
    LoadGlobal Width String
  | -- 操作スタック先頭をpopし、指定幅で static へストアする
    StoreGlobal Width String
  | -- static の実効アドレスを計算しpushする（&STATIC・static配列の添字アクセス 用）
    LoadGlobalAddr String
  deriving (Show, Eq)

-- Code generator

-- 名前が指す場所: ローカル変数・仮引数（%rbp相対オフセット）、static（シンボル名）、const（評価済みの値）
data Loc = LLocal Int | LStatic String | LConst Int
  deriving (Show, Eq)

-- 変数名 -> (場所, 型)。スコープのスタック（先頭が最内側）。
-- fn本体では最も外側（末尾）のスコープが const/static を集めたグローバルのスコープになる
type Env = [Map String (Loc, Type)]

-- スコープのスタック（[Map String v]、先頭が最内側）に対する共通ロジック

-- 先頭スコープから順に探し、無ければ外側へフォールバックする
lookupInScopes :: String -> [Map String v] -> Maybe v
lookupInScopes _ [] = Nothing
lookupInScopes name (scope : rest) =
  -- pTraceShow ("name", name) $
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupInScopes name rest

-- 先頭スコープ（現在のブロック）にのみ宣言されているかを判定する
memberInScope :: String -> [Map String v] -> Bool
memberInScope name (scope : _) = Map.member name scope
memberInScope _ [] = False

-- 先頭スコープにのみ値を挿入する
insertInScope :: String -> v -> [Map String v] -> [Map String v]
insertInScope name v (scope : rest) = Map.insert name v scope : rest
insertInScope _ _ [] = []

-- 先頭スコープから順に変数を探す（外側のスコープも参照できる）
lookupVar :: String -> Env -> Maybe (Loc, Type)
lookupVar = lookupInScopes

-- 先頭スコープ（現在のブロック）にのみ宣言されているかを判定する（シャドーイング判定用）
declaredLocally :: String -> Env -> Bool
declaredLocally = memberInScope

-- 先頭スコープにのみ変数を追加する
insertVar :: String -> (Loc, Type) -> Env -> Env
insertVar = insertInScope

-- let・仮引数で const/static と同名の変数を宣言しようとしていればエラー（Rustと同じく覆い隠しを禁止する）。
-- グローバルのスコープは Env の最も外側（末尾）にある
checkNotGlobal :: String -> Env -> Either String ()
checkNotGlobal _ [] = Right ()
checkNotGlobal name env
  | Map.member name (last env) = Left ("cannot shadow global: " ++ name)
  | otherwise = Right ()

-- 型ごとのスタック占有バイト数（物理格納幅）
widthBytes :: Width -> Int
widthBytes W32 = 4
widthBytes W64 = 8

-- 型の物理格納幅（bool・() は i32 用の32bitスロットを流用する。ポインタは指す先の型によらず常に8バイト）。
-- 配列は要素1個分の格納幅を返す（配列全体の占有バイト数は stackBytes を使う）
storageWidth :: Type -> Width
storageWidth (TyInt _ w) = w
storageWidth TUsize = W64
storageWidth TBool = W32
storageWidth TUnit = W32
storageWidth (TPtr _) = W64
storageWidth (TArray elemTy _) = storageWidth elemTy

-- 変数がスタック上に占有するバイト数（配列は 要素数 × 要素の格納幅）
stackBytes :: Type -> Int
stackBytes (TArray elemTy n) = n * widthBytes (storageWidth elemTy)
stackBytes ty = widthBytes (storageWidth ty)

-- エラーメッセージ表示用の型名
typeName :: Type -> String
typeName (TyInt Signed W32) = "i32"
typeName (TyInt Signed W64) = "i64"
typeName (TyInt Unsigned W32) = "u32"
typeName (TyInt Unsigned W64) = "u64"
typeName TUsize = "usize"
typeName TBool = "bool"
typeName TUnit = "()"
typeName (TPtr t) = "&" ++ typeName t
typeName (TArray t n) = "[" ++ typeName t ++ "; " ++ show n ++ "]"

-- 32bit整数の値を64bitスタック上の正規形（i32 は符号拡張、u32 はゼロ拡張）へ揃える命令
normalize32 :: Signedness -> Instr
normalize32 Signed = ISext32
normalize32 Unsigned = IZext32

-- 型 ty の値をスタックへ読み出した直後に必要な正規化命令（u32 のみ。Load/LoadInd W32 は符号拡張するため）
loadFixup :: Type -> [Instr]
loadFixup (TyInt Unsigned W32) = [IZext32]
loadFixup _ = []

-- 加減乗の結果を期待型の正規形へ揃える命令（i32 は IAdd W32 等が符号拡張済み、u32 のみゼロ拡張し直す）
arithFixup :: Signedness -> Width -> [Instr]
arithFixup Unsigned W32 = [IZext32]
arithFixup _ _ = []

-- 被演算子の型に応じた大小比較命令の選択（符号なし整数のみ U 版。符号あり整数・ポインタは従来の命令）
cmpInstr :: Type -> Instr -> Instr -> Instr
cmpInstr ty signedInstr unsignedInstr = case intRepr ty of
  Just (Unsigned, _) -> unsignedInstr
  _ -> signedInstr

-- 関数シグネチャ表: 名前 -> (仮引数の型リスト, 戻り値の型)。
-- 全fnの本体をコンパイルする前に一括構築する、コンパイル中不変のグローバルな読み取り専用テーブル
-- （宣言順に依存しない相互再帰を可能にするための二パスコンパイルの核）。
type FnSigs = Map String ([Type], Type)

-- SysV AMD64の整数引数レジスタはレジスタ渡し分の最大6個までしか無いため、
-- スタック経由の引数渡し（今回のスコープ外）を避けるべく上限として採用する
maxParams :: Int
maxParams = 6

-- グローバルの表: 名前 -> (場所, 型)。const は評価済みの値（LConst）、static はシンボル名（LStatic）を持つ。
-- FnSigs と同じく、全fnの本体をコンパイルする前に構築するコンパイル中不変のテーブル
type Globals = Map String (Loc, Type)

-- static の初期値の1要素: 整数、またはアドレスの定数（同じソースの static のシンボル名, バイトオフセット）
data StaticInit = SInt Int | SAddr String Int
  deriving (Show, Eq)

-- static の静的領域上の初期値: (シンボル名, 要素1個分の格納幅, 値の列（スカラーなら1個、配列なら要素数分）)
type StaticData = (String, Width, [StaticInit])

-- 1ソース分のコンパイル結果
data CompiledModule = CompiledModule
  { -- 自ソースで定義した static の初期値
    cmStatics :: [StaticData],
    -- main以外のfnの命令列
    cmFns :: [(String, [Instr])],
    -- mainがあれば (mainの戻り値型, mainの命令列)
    cmMain :: Maybe (Type, [Instr]),
    -- .globl で公開するシンボル（pub の fn・static の名前。main は codegen が常に公開する）
    cmPublic :: [String]
  }
  deriving (Show, Eq)

-- 単一ソースで完結するプログラム（import しない）をコンパイルする。main は必須。
-- 戻り値は (main以外のfnの命令列, mainの戻り値型, mainの命令列)。命令列を検査するテスト用で、
-- static の初期値は返さない（アセンブリまで組み立てるには compileModule と codegenModule を使う）
compile :: Program -> Either String ([(String, [Instr])], Type, [Instr])
compile prog = do
  validateFnShapes (progFns prog)
  when (null [() | FnDecl _ "main" _ _ _ <- progFns prog]) $ Left "missing function: main"
  m <- compileModule Map.empty Map.empty prog
  case cmMain m of
    Just (mainType, mainInstrs) -> Right (cmFns m, mainType, mainInstrs)
    Nothing -> Left "missing function: main"

-- 1ソース分の const/static・fn 定義から命令を抽出する（複数ソースからのビルドの単位）。
-- importedSigs/importedGlobals は import したソースに定義された pub の fn のシグネチャと const/static で、
-- 自ソースの定義と同様に参照できる（fn の本体や static の実体は別のオブジェクトファイルにあり、
-- リンク時に解決される）。自ソースの定義と同名ならエラー。
-- 各fnの本体は他の関数を一切参照しない独立スコープでコンパイルされ、
-- ラベル採番用のカウンタ（CompileM）は全fnを通じて単一のものを共有する
-- （同名ラベルの重複はアセンブル時に壊れるため）。
-- mainは戻り値を表示して終了する特別な末尾（codegen側のepilogue）を持つため、他のfnとは分けて返す。
-- main の有無はここでは検査しない（リンクする全ソースでちょうど1つあればよく、それはドライバが検査する）
compileModule :: FnSigs -> Globals -> Program -> Either String CompiledModule
compileModule importedSigs0 importedGlobals prog = do
  let fnDecls = progFns prog
  ownSigs <- buildFnSigs fnDecls
  (ownGlobals, statics) <- buildGlobals (progGlobals prog)
  -- fn・const・static は同じ名前空間を共有する
  case Map.keys (Map.intersection ownSigs ownGlobals) of
    (name : _) -> Left ("name already declared: " ++ name)
    [] -> Right ()
  -- import先の main は取り込まない（呼び出し不可であり、自ソースの main とも衝突させない）
  let importedSigs = Map.delete "main" importedSigs0
  case Map.keys (Map.intersection ownSigs importedSigs) of
    (name : _) -> Left ("function already declared in imported file: " ++ name)
    [] -> Right ()
  let ownNames = Map.union (Map.map (const ()) ownSigs) (Map.map (const ()) ownGlobals)
      importedNames = Map.union (Map.map (const ()) importedSigs) (Map.map (const ()) importedGlobals)
  case Map.keys (Map.intersection ownNames importedNames) of
    (name : _) -> Left ("name already declared in imported file: " ++ name)
    [] -> Right ()
  (fns, mainPart) <- evalStateT (compileProgram (Map.union ownSigs importedSigs) (Map.union ownGlobals importedGlobals) fnDecls) 0
  let public =
        [name | FnDecl Public name _ _ _ <- fnDecls]
          ++ [name | GlobalDecl Public GStatic name _ _ <- progGlobals prog]
  Right (CompiledModule statics fns mainPart public)

-- import したソースから使える（pub を付けた）fn・const・static の名前
publicNames :: Program -> [String]
publicNames prog =
  [name | FnDecl Public name _ _ _ <- progFns prog]
    ++ [name | GlobalDecl Public _ name _ _ <- progGlobals prog]

compileProgram :: FnSigs -> Globals -> [FnDecl] -> CompileM ([(String, [Instr])], Maybe (Type, [Instr]))
compileProgram fnSigs globals fnDecls = do
  fns <- mapM (compileFnDecl fnSigs globals) fnDecls
  let mainPart = do
        mainInstrs <- lookup "main" fns
        (_, mainType) <- Map.lookup "main" fnSigs
        Just (mainType, mainInstrs)
  pure ([fn | fn@(name, _) <- fns, name /= "main"], mainPart)

-- if の分岐ラベル採番用のカウンタを持ち回るモナド。
-- Env/cursor はブロックやif分岐を抜けるたびに「呼び出し前の値へ巻き戻す」必要がある一方、
-- ラベル番号は逆に「巻き戻してはいけない」（同名ラベルの重複はアセンブル時に壊れる）。
-- この非対称性を素朴なタプル要素として持ち回ると、SBlock のように戻り値のEnv/cursorを
-- 握りつぶす実装をうっかりコピーしてラベルカウンタまで一緒に握りつぶす事故が起きやすい。
-- StateT の状態として分離しておけば、Env/cursorをどう扱おうと状態は常に `>>=` の鎖に沿って
-- 素通しされるため、この種の事故が構造的に起こらない。
type CompileM = StateT Int (Either String)

-- 新しい一意なラベル名を払い出す（.L はGASのローカルラベル慣習に合わせたプレフィックス）
freshLabel :: String -> CompileM String
freshLabel prefix = do
  n <- get
  put (n + 1)
  -- pTraceShowM ("freshLabel" :: String, ".L" ++ prefix ++ show n)
  pure (".L" ++ prefix ++ show n)

-- 直近の外側ループの (continueラベル, breakラベル)。ループの外側では Nothing であり、
-- break/continueの使用はコンパイルエラーとなる。
-- Env と同じく普通の関数引数として渡す（ネストしたwhileに入るときだけ新しい値に差し替え、
-- 呼び出しから戻れば自動的に元の値に戻る）。ラベルカウンタ（StateT）と違って「巻き戻してはいけない」
-- 状態ではなく、逆に「ブロック/ifを跨いでも外側ループの値を保ち続け、ループを抜けたら消える」
-- スコープ的な情報なので、cursor/Envと同じ素朴な引数渡しがそのまま正しい挙動を与える。
type LoopCtx = Maybe (String, String)

-- 現在コンパイル中の関数の (関数名, 戻り値の型, 関数末尾ラベル)。関数の外側では Nothing であり、
-- returnの使用はコンパイルエラーとなる（全ての文はfn本体の中にあるため、現在は常に Just）。LoopCtxと同じ理由で普通の関数引数として渡す：
-- ブロック/if分岐/while本体へは変更せずそのまま素通しし（returnは常に直近の外側“関数”から戻るため、
-- ループを跨いでも値を保ち続ける必要がある）、関数本体に入るときだけ新しい値に差し替える。
type ReturnCtx = Maybe (String, Type, String)

-- 任意のEnv/cursor/loopCtx/returnCtxを起点に[文]から命令を抽出する（ブロック/if分岐/while本体/関数本体の再帰コンパイルに使う）
compileStmtsFrom :: FnSigs -> Env -> Int -> LoopCtx -> ReturnCtx -> [Stmt] -> CompileM (Env, Int, [Instr])
compileStmtsFrom fnSigs initEnv initCursor loopCtx returnCtx stmts = foldM step (initEnv, initCursor, []) stmts
  where
    -- let xxx: 型 = ...
    step (env, cursor, acc) (SLet name ty expr) = do
      -- 変数の二重定義をチェック（同一ブロック内の再宣言のみ対象。外側との同名はシャドーイングとして許可）
      when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
      lift (checkNotGlobal name env)
      -- 新しく登録する変数のスタック上のアドレスを、型の占有バイト数分だけ詰めて計算
      let off = cursor - stackBytes ty
      -- 式の表現から命令を抽出（宣言された型を期待型として渡す）＋変数のストア
      instrs <- lift (compileLetInit fnSigs env ty off expr)
      -- 変数とアドレスのマップ, 命令＋追加命令
      pure (insertVar name (LLocal off, ty) env, off, acc ++ instrs)
    -- let xxx = ...（型注釈省略。inferTypeで推論し、最後まで未確定ならi64をデフォルトとする）
    step (env, cursor, acc) (SLetInferred name expr) = do
      when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
      lift (checkNotGlobal name env)
      ty <- lift (inferType fnSigs env expr)
      let off = cursor - stackBytes ty
      instrs <- lift (compileLetInit fnSigs env ty off expr)
      pure (insertVar name (LLocal off, ty) env, off, acc ++ instrs)
    -- xxx = ...
    step (env, cursor, acc) (SAssign name expr) = do
      -- 変数の定義をチェック（外側スコープの変数への書き込みも許可）
      (loc, ty) <- lift (maybe (Left ("undeclared variable: " ++ name)) Right (lookupVar name env))
      -- 配列全体への代入は不可（要素ごとに代入する）
      case ty of
        TArray _ _ -> lift (Left ("cannot assign to array " ++ name ++ "; assign to elements instead"))
        _ -> pure ()
      -- ストア先は場所によって切り替える（const はメモリを持たないため代入できない）
      storeInstr <- lift $ case loc of
        LLocal off -> Right (Store (storageWidth ty) off)
        LStatic sym -> Right (StoreGlobal (storageWidth ty) sym)
        LConst _ -> Left ("cannot assign to constant: " ++ name)
      lift (checkNoLocalStore env name expr)
      -- 式の表現から命令を抽出（既存の変数の型を期待型として渡す）
      instrs <- lift (compileExprTyped fnSigs env ty expr)
      -- 変数とアドレスのマップはそのまま, 命令＋追加命令＋変数のストア
      pure (env, cursor, acc ++ instrs ++ [storeInstr])
    -- a[i] = ... / p[i] = ... / *p = ...（場所式のアドレスを計算し、その型を期待型として値をコンパイルしてから間接ストアする）
    step (env, cursor, acc) (SAssignPlace lhs expr) = do
      case lhs of
        Index base _ -> case base of
          Var x -> lift (checkNoLocalStore env x expr)
          _ -> pure ()
        Deref _ -> pure ()
        _ -> lift (Left "invalid left-hand side of assignment")
      (ty, addrInstrs) <- lift (addressOf fnSigs env lhs)
      case ty of
        TArray _ _ -> lift (Left "cannot assign to array; assign to elements instead")
        _ -> pure ()
      instrs <- lift (compileExprTyped fnSigs env ty expr)
      pure (env, cursor, acc ++ addrInstrs ++ instrs ++ [StoreInd (storageWidth ty)])
    -- { ... }
    step (env, cursor, acc) (SBlock innerStmts) = do
      -- 先頭に空スコープをpushして再帰コンパイルし、返り値のEnv/cursorは破棄して呼び出し前の値をそのまま継続に使う
      -- （＝スコープアウトとスタックオフセットの巻き戻しを同時に実現する）。loopCtx/returnCtxはそのまま素通しする
      -- （ブロックにネストしても外側ループのbreak/continue・外側関数のreturnが引き続き解決できるようにするため）
      (_, _, instrs) <- compileStmtsFrom fnSigs (Map.empty : env) cursor loopCtx returnCtx innerStmts
      pure (env, cursor, acc ++ instrs)
    -- if 式 {...} (else if 式 {...})* (else {...})?
    step (env, cursor, acc) (SIf branches maybeElse) = do
      instrs <- compileIf fnSigs env cursor loopCtx returnCtx branches maybeElse
      pure (env, cursor, acc ++ instrs)
    -- while 式 {...}
    step (env, cursor, acc) (SWhile cond body) = do
      instrs <- compileWhile fnSigs env cursor returnCtx cond body
      pure (env, cursor, acc ++ instrs)
    -- break;
    step (env, cursor, acc) SBreak = do
      lbl <- lift (maybe (Left "break used outside loop") (Right . snd) loopCtx)
      pure (env, cursor, acc ++ [Jmp lbl])
    -- continue;
    step (env, cursor, acc) SContinue = do
      lbl <- lift (maybe (Left "continue used outside loop") (Right . fst) loopCtx)
      pure (env, cursor, acc ++ [Jmp lbl])
    -- return expr;
    step (env, cursor, acc) (SReturn expr) = do
      (fnName, retTy, endLabel) <- lift (maybe (Left "return used outside function") Right returnCtx)
      lift (checkNoLocalRef fnName env expr)
      instrs <- lift (compileExprTyped fnSigs env retTy expr)
      pure (env, cursor, acc ++ instrs ++ [Jmp endLabel])
    -- expr;（letの型注釈省略と同じ規則で型を決めてコンパイルし、値は捨てる）
    step (env, cursor, acc) (SExpr expr) = do
      ty <- lift (inferType fnSigs env expr)
      instrs <- lift (compileExprTyped fnSigs env ty expr)
      pure (env, cursor, acc ++ instrs ++ [IPop])

-- let の初期化式をコンパイルし、オフセット off の変数へストアする命令列を返す。
-- 配列型は要素ごとのストア（compileArrayInit）、それ以外は式を評価して1回ストアする
compileLetInit :: FnSigs -> Env -> Type -> Int -> Expr -> Either String [Instr]
compileLetInit fnSigs env (TArray elemTy len) off expr = compileArrayInit fnSigs env elemTy len off expr
compileLetInit fnSigs env ty off expr = do
  instrs <- compileExprTyped fnSigs env ty expr
  Right (instrs ++ [Store (storageWidth ty) off])

-- if/else-if/else の分岐列を、条件が偽なら次の分岐へジャンプする形の命令列へ展開する。
-- 各分岐の本体はブロックと同じく独立スコープでコンパイルし、Env/cursorは呼び出し側へ伝播させない
-- （if全体も値を返さない文であり、外側から見た変数の状態はifに入る前と変わらない）。
-- loopCtx/returnCtxはそのまま素通しする（if本体にネストしても外側ループのbreak/continue・
-- 外側関数のreturnが解決できるようにするため）。
compileIf :: FnSigs -> Env -> Int -> LoopCtx -> ReturnCtx -> [(Expr, [Stmt])] -> Maybe [Stmt] -> CompileM [Instr]
compileIf fnSigs env cursor loopCtx returnCtx branches maybeElse = do
  -- pTraceShowM ("branches", branches)
  endLabel <- freshLabel "if_end"
  body <- go endLabel branches
  pure (body ++ [Label endLabel])
  where
    go _ [] = case maybeElse of
      Nothing -> pure []
      Just elseStmts -> do
        (_, _, instrs) <- compileStmtsFrom fnSigs (Map.empty : env) cursor loopCtx returnCtx elseStmts
        pure instrs
    go endLabel ((cond, body) : rest) = do
      -- 条件式は式木としては expected とは独立にbool型を要求する（if自体はexprを持たない）
      condInstrs <- lift (compileExprTyped fnSigs env TBool cond)
      nextLabel <- freshLabel "if_next"
      (_, _, bodyInstrs) <- compileStmtsFrom fnSigs (Map.empty : env) cursor loopCtx returnCtx body
      restInstrs <- go endLabel rest
      pure
        ( condInstrs
            ++ [JmpIfZero nextLabel]
            ++ bodyInstrs
            ++ [Jmp endLabel, Label nextLabel]
            ++ restInstrs
        )

-- while を「先頭で条件を検査し、真なら本体を実行して先頭へ戻る」形の命令列へ展開する。
-- 本体はブロックと同じく独立スコープでコンパイルし、Env/cursorは呼び出し側へ伝播させない。
-- 本体コンパイルへ渡すloopCtxは常にこのwhile自身の(開始,終了)ラベルで上書きする
-- （呼び出し元のloopCtxを受け取らないことで、ネストしたwhileのbreak/continueが必ず
-- 直近の内側ループへ解決される）。returnCtxはそのまま素通しする。
compileWhile :: FnSigs -> Env -> Int -> ReturnCtx -> Expr -> [Stmt] -> CompileM [Instr]
compileWhile fnSigs env cursor returnCtx cond body = do
  startLabel <- freshLabel "while_start"
  endLabel <- freshLabel "while_end"
  -- 条件式はifと同様、expectedとは独立にbool型を要求する
  condInstrs <- lift (compileExprTyped fnSigs env TBool cond)
  (_, _, bodyInstrs) <- compileStmtsFrom fnSigs (Map.empty : env) cursor (Just (startLabel, endLabel)) returnCtx body
  pure
    ( [Label startLabel]
        ++ condInstrs
        ++ [JmpIfZero endLabel]
        ++ bodyInstrs
        ++ [Jmp startLabel, Label endLabel]
    )

-- 仮引数を let と同じ規則でスタックスロットに割り付ける（宣言順どおりに詰める）。
-- 戻り値: (宣言順の(名前, オフセット, 型)のリスト, 変換後のEnv用スコープ, 最終cursor)
allocParams :: [(String, Type)] -> ([(String, Int, Type)], Map String (Loc, Type), Int)
allocParams params = (reverse revAssigned, Map.fromList [(n, (LLocal o, t)) | (n, o, t) <- revAssigned], cursor)
  where
    (revAssigned, cursor) = foldl step ([], 0) params
    step (acc, c) (name, ty) =
      let off = c - stackBytes ty
       in ((name, off, ty) : acc, off)

-- 関数本体のコンパイル。パラメータのみを含む独立スコープ（他の関数の変数は
-- 一切参照できない。その外側にグローバルのスコープだけがある）から開始し、パラメータはプロローグ直後に
-- レジスタからスタックへスピルする（StoreArg）。早期returnと末尾式のフォールスルーは同じ関数末尾ラベルへ合流する
compileFnDecl :: FnSigs -> Globals -> FnDecl -> CompileM (String, [Instr])
compileFnDecl fnSigs globals (FnDecl _ name params retTy (stmts, tailExpr)) = do
  endLabel <- freshLabel "fn_end"
  lift (mapM_ (\(p, _) -> checkNotGlobal p [globals]) params)
  let (assigned, paramEnv, paramCursor) = allocParams params
      env0 = [paramEnv, globals]
      spillInstrs = [StoreArg i (storageWidth ty) off | (i, (_, off, ty)) <- zip [0 ..] assigned]
      returnCtx = Just (name, retTy, endLabel)
  (env1, _cursor1, stmtInstrs) <- compileStmtsFrom fnSigs env0 paramCursor Nothing returnCtx stmts
  lift (checkNoLocalRef name env1 tailExpr)
  tailInstrs <- lift (compileExprTyped fnSigs env1 retTy tailExpr)
  -- pTraceShowM ("assigned of " ++ name, assigned)
  -- pTraceShowM ("env0 of " ++ name, env0)
  -- pTraceShowM ("env1 of " ++ name, env1)
  pure (name, spillInstrs ++ stmtInstrs ++ tailInstrs ++ [Label endLabel])

-- 関数から返す式（return式・末尾式）が、直接その関数のローカル変数（仮引数を含む）を指すポインタで
-- あればエラーとする（関数から戻るとスタックフレームが破棄され、ダングリングポインタになるため）。
-- 検出するのは `&x` と `&a[i]`（aがローカルの配列）を直接返す形のみの簡易検査で、中間変数を経由する
-- `let p = &x; p` などは検出しない。`&*p` や `&p[i]`（pがポインタ）は呼び出し元のデータを指すため許可する。
-- main は対象外（main の戻り値は main 自身のスタックフレーム上で表示してから終了するため、ダングリングにならない）
checkNoLocalRef :: String -> Env -> Expr -> Either String ()
checkNoLocalRef "main" _ _ = Right ()
checkNoLocalRef _ env expr = case isLocalRef env expr of
  Just name -> Left ("cannot return reference to local variable: " ++ name)
  Nothing -> Right ()

-- static への代入（`P = e;` / `PS[i] = e;`）で、右辺が直接ローカル変数を指すポインタであればエラーとする
-- （static はプログラムの終了まで残るため、関数から戻るとダングリングポインタになる）。
-- checkNoLocalRef と同じ簡易検査で、中間変数を経由する形やポインタ経由の代入は検出しない。
-- main も対象とする（main では実害は無いが、例外を設けず規則を単純にする）
checkNoLocalStore :: Env -> String -> Expr -> Either String ()
checkNoLocalStore env target expr = case lookupVar target env of
  Just (LStatic _, _) | Just _ <- isLocalRef env expr -> Left ("cannot store reference to local variable in static: " ++ target)
  _ -> Right ()

-- 式が直接ローカル変数（仮引数を含む）を指すポインタ（`&x` / `&a[i]`。a はローカルの配列）であれば、その変数名を返す
isLocalRef :: Env -> Expr -> Maybe String
isLocalRef env (AddrOf place) =
  case place of
    Var name | Just (LLocal _, _) <- lookupVar name env -> Just name
    Index (Var name) _ | Just (LLocal _, TArray _ _) <- lookupVar name env -> Just name
    _ -> Nothing
isLocalRef _ _ = Nothing

-- Maybe Type の単一化（Nothing = 整数リテラルなど未確定な部分木）
unifyType :: Type -> Type -> Either String Type
unifyType t1 t2
  | t1 == t2 = Right t1
  | otherwise = Left ("type mismatch: " ++ typeName t1 ++ " and " ++ typeName t2)

unifyMaybeType :: Maybe Type -> Maybe Type -> Either String (Maybe Type)
unifyMaybeType Nothing Nothing = Right Nothing
unifyMaybeType Nothing (Just t) = Right (Just t)
unifyMaybeType (Just t) Nothing = Right (Just t)
unifyMaybeType (Just t1) (Just t2) = Just <$> unifyType t1 t2

-- 式中の変数参照・関数呼び出しから型を推論する（整数リテラルのみの部分木は Nothing = 未確定のまま単一化する）。
-- bool リテラルは曖昧さがないため常に Just TBool。
inferMaybeType :: FnSigs -> Env -> Expr -> Either String (Maybe Type)
inferMaybeType fnSigs env expr = do
  -- pTraceShowM ("inferMaybeType実行", expr)
  go expr
  where
    go (Lit _) = Right Nothing
    -- サフィックス付きリテラルは Var と同様、常に確定した型を持つ（expectedとは無関係）
    go (LitTyped _ ty) = Right (Just ty)
    go (BoolLit _) = Right (Just TBool)
    go UnitLit = Right (Just TUnit)
    go (Var name) =
      maybe (Left ("undeclared variable: " ++ name)) (Right . Just . snd) (lookupVar name env)
    go (Neg e) = go e
    go (Not e) = do
      t <- go e
      case t of
        Just ty | isIntType ty -> Left ("type mismatch: expected bool, found " ++ typeName ty)
        _ -> Right (Just TBool)
    go (Add a b) = combine a b
    go (Sub a b) = combine a b
    go (Mul a b) = combine a b
    go (Div a b) = combine a b
    -- Eq/Neq は被演算子同士の型を単一化するが、ノード自体の型は常に TBool
    -- （算術演算と異なり、被演算子の型と結果の型が一致しない）
    go (Eq a b) = combine a b >> Right (Just TBool)
    go (Neq a b) = combine a b >> Right (Just TBool)
    go (Lt a b) = combine a b >> Right (Just TBool)
    go (Le a b) = combine a b >> Right (Just TBool)
    go (Gt a b) = combine a b >> Right (Just TBool)
    go (Ge a b) = combine a b >> Right (Just TBool)
    -- キャスト（as i64/as i32）の結果型は被演算子によらず常に確定する（BoolLit と同様、部分木を辿る必要はない）
    go (Cast _ ty) = Right (Just ty)
    -- 配列リテラルの型は要素の型を単一化したもの（全要素が無型リテラルなら i64）を要素型とする配列
    go (ArrayLit es) = do
      elemTy <- foldM (\acc e -> go e >>= unifyMaybeType acc) Nothing es
      Right (Just (TArray (maybe (TyInt Signed W64) id elemTy) (length es)))
    go (ArrayRepeat e n) = do
      elemTy <- go e
      Right (Just (TArray (maybe (TyInt Signed W64) id elemTy) n))
    -- a[i] の型は配列の要素型（p[i] のように配列へのポインタも自動デリファレンスする）
    go (Index base _) = do
      t <- go base
      case t of
        Just (TArray elemTy _) -> Right (Just elemTy)
        Just (TPtr (TArray elemTy _)) -> Right (Just elemTy)
        Just other -> Left ("cannot index into a value of type " ++ typeName other)
        Nothing -> Left "cannot index into a value of type integer literal"
    -- &lvalue の型は lvalue 自身の型を TPtr で包んだもの（addressOf に委譲する）
    go (AddrOf e) = do
      (pointeeTy, _) <- addressOf fnSigs env e
      Right (Just (TPtr pointeeTy))
    --  *ptr の型は ptr 自身の型からポインタを一枚剥がしたもの
    go (Deref e) = do
      t <- go e
      case t of
        Just (TPtr inner) -> Right (Just inner)
        Just other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
        Nothing -> Left "type mismatch: cannot dereference an untyped literal"
    -- 呼び出しの型は常にシグネチャの戻り値型で確定する（算術演算と異なり、引数の型を
    -- 単一化する必要はない。各引数の型検査はcompileExprTyped側で行う）
    go (Call "main" _) = Left "cannot call main"
    go (Call name args) =
      case Map.lookup name fnSigs of
        Nothing -> Left ("undeclared function: " ++ name)
        Just (paramTys, retTy)
          | length paramTys /= length args ->
              Left
                ( "wrong number of arguments for "
                    ++ name
                    ++ ": expected "
                    ++ show (length paramTys)
                    ++ ", found "
                    ++ show (length args)
                )
          | otherwise -> Right (Just retTy)
    combine a b = do
      ta <- go a
      tb <- go b
      unifyMaybeType ta tb

-- 最後まで未確定なら i64 をデフォルトとする。
inferType :: FnSigs -> Env -> Expr -> Either String Type
inferType fnSigs env expr = maybe (TyInt Signed W64) id <$> inferMaybeType fnSigs env expr

-- Eq/Neq の被演算子同士の共通の型を決定する（外側の expected とは独立に推論する）
operandType :: FnSigs -> Env -> Expr -> Expr -> Either String Type
operandType fnSigs env a b = do
  ta <- inferMaybeType fnSigs env a
  tb <- inferMaybeType fnSigs env b
  maybe (TyInt Signed W64) id <$> unifyMaybeType ta tb

-- lvalue（&の対象・代入の左辺になれる式）の実効アドレスを計算する命令列と、その参照先の型を返す。
-- lvalueとして認めるのは変数参照（配列を含む）、ポインタのデリファレンス（&*p は p 自身の値が
-- 既にアドレスそのものなので LoadAddr/LoadInd を使わず p を普通にコンパイルするだけでよい）、配列要素の3種のみ。
-- それ以外（リテラル・算術式・関数呼び出し・&式自身 等）は意味論エラーとする。
addressOf :: FnSigs -> Env -> Expr -> Either String (Type, [Instr])
addressOf _ env (Var name) =
  case lookupVar name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (LLocal off, ty) -> Right (ty, [LoadAddr off])
    Just (LStatic sym, ty) -> Right (ty, [LoadGlobalAddr sym])
    -- const はメモリを持たないため、アドレスを取れない
    Just (LConst _, _) -> Left ("cannot take address of constant: " ++ name)
-- &a[i] は要素の実効アドレス
addressOf fnSigs env (Index base idx) = indexAddr fnSigs env base idx
addressOf fnSigs env (Deref e) = do
  t <-
    inferMaybeType fnSigs env e
      >>= maybe (Left "type mismatch: cannot take address of a dereferenced untyped literal") Right
  case t of
    TPtr inner -> (,) inner <$> compileExprTyped fnSigs env t e
    other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
addressOf _ _ e = Left ("invalid operand for &: not an lvalue: " ++ show e)

-- 構造検証: 重複定義・最大引数数・mainの引数なし（型の中身は見ない。mainの存在は検査しない）
validateFnShapes :: [FnDecl] -> Either String ()
validateFnShapes fnDecls = do
  _ <- foldM addSig Map.empty fnDecls
  pure ()
  where
    addSig :: Map String () -> FnDecl -> Either String (Map String ())
    addSig seen (FnDecl _ name params _ _)
      | Map.member name seen = Left ("function already declared: " ++ name)
      | length params > maxParams = Left ("too many parameters (max " ++ show maxParams ++ "): " ++ name)
      -- main は呼び出し元を持たず、戻り値を表示して終了するだけなので、引数を受け取る経路が無い
      | name == "main" && not (null params) = Left "main function must not take parameters"
      | otherwise = Right (Map.insert name () seen)

-- 全fn定義からFnSigsを一括で作る（本体のコンパイルより前に行う1パス目）。
-- シグネチャは宣言だけで確定する（仮引数の型注釈は必須、'->' 省略は ()）ため、関数本体や呼び出し箇所は見ない。
-- 他のソースのfnも宣言さえあれば呼び出せるようにするための前提（複数ソースからのビルド）
buildFnSigs :: [FnDecl] -> Either String FnSigs
buildFnSigs fnDecls = do
  validateFnShapes fnDecls
  let sigs = [(name, (map snd params, retTy)) | FnDecl _ name params retTy _ <- fnDecls]
  -- 配列はローカル変数専用のため、仮引数・戻り値の型には使えない
  mapM_
    ( \(name, (paramTys, retTy)) ->
        when (any isArrayType (retTy : paramTys)) $
          Left ("array type is not allowed in function signature (pass &[T; N] instead): " ++ name)
    )
    sigs
  pure (Map.fromList sigs)
  where
    isArrayType (TArray _ _) = True
    isArrayType _ = False

-- 1ソース分の const/static の定義からグローバルの表と static の初期値を作る。
-- 型を検査し、初期値（定数式）をコンパイル時に評価する。定数式から参照できるのは同じソースの const だけで
-- （import したソースの const は参照できない）、宣言順によらず参照できるよう依存する const を先に評価する
buildGlobals :: [GlobalDecl] -> Either String (Globals, [StaticData])
buildGlobals decls = do
  _ <- foldM addName Map.empty decls
  mapM_ checkGlobalType decls
  let constDecls = Map.fromList [(name, (ty, e)) | GlobalDecl _ GConst name ty e <- decls]
      staticTypes = Map.fromList [(name, ty) | GlobalDecl _ GStatic name ty _ <- decls]
      staticNames = Map.keys staticTypes
  consts <- foldM (evalConst constDecls staticNames []) Map.empty (Map.keys constDecls)
  statics <- mapM (evalStatic consts staticTypes) [(name, ty, e) | GlobalDecl _ GStatic name ty e <- decls]
  let globals =
        Map.union
          (Map.map (\(v, ty) -> (LConst v, ty)) consts)
          (Map.fromList [(name, (LStatic name, ty)) | GlobalDecl _ GStatic name ty _ <- decls])
  Right (globals, statics)
  where
    addName seen (GlobalDecl _ _ name _ _)
      | Map.member name seen = Left ("name already declared: " ++ name)
      | otherwise = Right (Map.insert name () seen)
    -- const はメモリを持たず値を Push 1つで埋め込むためスカラーのみ。static はスカラー・ポインタとその配列
    -- （ポインタの初期値は static のアドレスの定数。evalAddrConst 参照）
    isScalar ty = isIntType ty || ty == TBool || ty == TUnit
    isStaticElem ty = isScalar ty || isPtrType ty
    isPtrType (TPtr _) = True
    isPtrType _ = False
    checkGlobalType (GlobalDecl _ GConst name ty _)
      | isScalar ty = Right ()
      | otherwise = Left ("invalid type for constant: " ++ name)
    checkGlobalType (GlobalDecl _ GStatic name ty _) = case ty of
      TArray elemTy _ | isStaticElem elemTy -> Right ()
      _ | isStaticElem ty -> Right ()
      _ -> Left ("invalid type for static: " ++ name)

-- const name を（依存する const を先に評価してから）評価し、評価済みの表へ加える。
-- visiting は評価中の const の列で、そこへ戻ってきたら循環とする
evalConst :: Map String (Type, Expr) -> [String] -> [String] -> Map String (Int, Type) -> String -> Either String (Map String (Int, Type))
evalConst constDecls staticNames visiting done name
  | Map.member name done = Right done
  | name `elem` visiting = Left ("cycle in constant definitions: " ++ name)
  | otherwise = case Map.lookup name constDecls of
      -- const 以外の名前（未宣言の変数など）は評価時の compileExprTyped がエラーにする
      Nothing -> Right done
      Just (ty, e) -> do
        checkConstExpr staticNames name e
        done' <- foldM (evalConst constDecls staticNames (name : visiting)) done (exprVars e)
        v <- evalConstExpr done' name ty e
        Right (Map.insert name (v, ty) done')

-- static の初期値を評価する（配列は要素ごと。[e; N] は e を1回評価して N 個並べる）。
-- staticTypes は同じソースの static の型の表（定数式からの参照の禁止と、アドレスの定数の解決に使う）
evalStatic :: Map String (Int, Type) -> Map String Type -> (String, Type, Expr) -> Either String StaticData
evalStatic consts staticTypes (name, ty, e) = case ty of
  TArray elemTy len -> do
    vs <- case e of
      ArrayLit es
        | length es /= len -> lengthMismatch len (length es)
        | otherwise -> mapM (evalElem elemTy) es
      ArrayRepeat e1 k
        | k /= len -> lengthMismatch len k
        | otherwise -> replicate len <$> evalElem elemTy e1
      _ -> Left "array must be initialized with an array literal"
    Right (name, storageWidth elemTy, vs)
  _ -> do
    v <- evalElem ty e
    Right (name, storageWidth ty, [v])
  where
    -- ポインタ型の要素はアドレスの定数、それ以外は定数式として評価する
    evalElem (TPtr pointeeTy) e1 = evalAddrConst consts staticTypes name pointeeTy e1
    evalElem elemTy e1 = do
      checkConstExpr (Map.keys staticTypes) name e1
      SInt <$> evalConstExpr consts name elemTy e1
    lengthMismatch len found =
      Left ("array length mismatch: expected " ++ show len ++ ", found " ++ show found)

-- ポインタ型の static の初期値（アドレスの定数）を評価する。書けるのは同じソースの static の
-- `&X` と `&T[i]`（T は static の配列、i は定数式）だけ。アドレスはリンク時（PIEならロード時）まで決まらないため、
-- VM では評価せずシンボル名とバイトオフセットの組にする
evalAddrConst :: Map String (Int, Type) -> Map String Type -> String -> Type -> Expr -> Either String StaticInit
evalAddrConst consts staticTypes name pointeeTy expr = case expr of
  AddrOf (Var x) -> do
    xTy <- lookupTarget x
    expectPointee xTy
    Right (SAddr x 0)
  AddrOf (Index (Var x) idx) -> do
    xTy <- lookupTarget x
    case xTy of
      TArray elemTy len -> do
        expectPointee elemTy
        checkConstExpr (Map.keys staticTypes) name idx
        i <- evalConstExpr consts name TUsize idx
        when (toU64 i >= fromIntegral len) $
          Left ("index out of bounds: the length is " ++ show len ++ " but the index is " ++ show (toU64 i))
        Right (SAddr x (i * widthBytes (storageWidth elemTy)))
      _ -> notConst
  _ -> notConst
  where
    lookupTarget x = case Map.lookup x staticTypes of
      Just t -> Right t
      Nothing
        | Map.member x consts -> Left ("cannot take address of constant: " ++ x)
        | otherwise -> Left ("undeclared variable: " ++ x)
    expectPointee t
      | t == pointeeTy = Right ()
      | otherwise = Left ("type mismatch: expected " ++ typeName (TPtr pointeeTy) ++ ", found " ++ typeName (TPtr t))
    notConst = Left ("initializer of " ++ name ++ " is not a constant expression")

-- 定数式として書ける式（リテラル・変数参照・単項/二項演算・キャスト）だけで構成されているかを検査する。
-- static は実行時に値が変わるため参照できない
checkConstExpr :: [String] -> String -> Expr -> Either String ()
checkConstExpr staticNames name = go
  where
    go (Lit _) = Right ()
    go (LitTyped _ _) = Right ()
    go (BoolLit _) = Right ()
    go UnitLit = Right ()
    go (Var v)
      | v `elem` staticNames = notConst
      | otherwise = Right ()
    go (Neg e) = go e
    go (Not e) = go e
    go (Cast e _) = go e
    go (Add a b) = go a >> go b
    go (Sub a b) = go a >> go b
    go (Mul a b) = go a >> go b
    go (Div a b) = go a >> go b
    go (Eq a b) = go a >> go b
    go (Neq a b) = go a >> go b
    go (Lt a b) = go a >> go b
    go (Le a b) = go a >> go b
    go (Gt a b) = go a >> go b
    go (Ge a b) = go a >> go b
    go _ = notConst
    notConst = Left ("initializer of " ++ name ++ " is not a constant expression")

-- 定数式が参照する変数名（checkConstExpr を通った式のみを対象とする）
exprVars :: Expr -> [String]
exprVars (Var v) = [v]
exprVars (Neg e) = exprVars e
exprVars (Not e) = exprVars e
exprVars (Cast e _) = exprVars e
exprVars (Add a b) = exprVars a ++ exprVars b
exprVars (Sub a b) = exprVars a ++ exprVars b
exprVars (Mul a b) = exprVars a ++ exprVars b
exprVars (Div a b) = exprVars a ++ exprVars b
exprVars (Eq a b) = exprVars a ++ exprVars b
exprVars (Neq a b) = exprVars a ++ exprVars b
exprVars (Lt a b) = exprVars a ++ exprVars b
exprVars (Le a b) = exprVars a ++ exprVars b
exprVars (Gt a b) = exprVars a ++ exprVars b
exprVars (Ge a b) = exprVars a ++ exprVars b
exprVars _ = []

-- 定数式を、評価済みの const だけを含む Env で通常の式と同じくコンパイルし（型検査も同じ規則）、
-- VM で実行して値を得る。結果は型の正規形（i32 は符号拡張、u32 はゼロ拡張）へ揃える
evalConstExpr :: Map String (Int, Type) -> String -> Type -> Expr -> Either String Int
evalConstExpr consts name ty e = do
  instrs <- compileExprTyped Map.empty [Map.map (\(v, t) -> (LConst v, t)) consts] ty e
  case run instrs of
    Left "division by zero" -> Left ("division by zero in constant expression: " ++ name)
    Left err -> Left err
    Right v -> Right $ case intRepr ty of
      Just (Signed, W32) -> trunc W32 v
      Just (Unsigned, W32) -> zext32 v
      _ -> v

-- 期待する型（expected）を一様に伝播させながら命令を抽出する。
-- Var の実際の型が expected と食い違えば型不一致エラーとする。
-- 算術演算・整数リテラルは整数型（intRepr が Just を返す型）専用、Not/BoolLit/Eq/Neq は TBool 専用であり、
-- expected と食い違えばその場でエラーとする。配列型は値として扱えない（letの初期化は compileArrayInit が担う）。
compileExprTyped :: FnSigs -> Env -> Type -> Expr -> Either String [Instr]
-- [let ]xxx = 1234;（リテラルは無型なので expected をそのまま採用する。ただし整数型以外は不可）
compileExprTyped _ _ expected (Lit n)
  | isIntType expected = Right [Push n]
  | otherwise = Left ("type mismatch: expected " ++ typeName expected ++ ", found integer literal")
-- [let ]xxx = 1234i64; / 1234usize; など（サフィックスで型が確定済み。Varと同様、expectedと食い違えばエラー）
compileExprTyped _ _ expected (LitTyped n ty)
  | expected == ty = Right [Push n]
  | otherwise = Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName ty)
-- [let ]xxx = true; / false;（bool以外のコンテキストでは不可）
compileExprTyped _ _ TBool (BoolLit b) = Right [Push (if b then 1 else 0)]
compileExprTyped _ _ expected (BoolLit _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
-- [let ]xxx = ();（ユニット値は常に0として積む）
compileExprTyped _ _ TUnit UnitLit = Right [Push 0]
compileExprTyped _ _ expected UnitLit =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found ()")
-- [let ]xxx = yyy;
compileExprTyped _ env expected (Var name) =
  case lookupVar name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (_, TArray _ _) -> Left ("array cannot be used as a value: " ++ name)
    Just (loc, ty)
      | ty /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName ty)
      | otherwise -> Right $ case loc of
          LLocal off -> Load (storageWidth ty) off : loadFixup ty
          LStatic sym -> LoadGlobal (storageWidth ty) sym : loadFixup ty
          -- const は評価済み（型の正規形へ揃え済み）の値をそのまま積む
          LConst v -> [Push v]
-- [let ]xxx = expr + expr; / - / *（加減乗は符号の有無で下位ビットが変わらないため命令を共用し、u32のみ正規化し直す）
compileExprTyped fnSigs env expected (Add l r) = compileArith fnSigs env expected l r (\sg w -> IAdd w : arithFixup sg w)
compileExprTyped fnSigs env expected (Sub l r) = compileArith fnSigs env expected l r (\sg w -> ISub w : arithFixup sg w)
compileExprTyped fnSigs env expected (Mul l r) = compileArith fnSigs env expected l r (\sg w -> IMul w : arithFixup sg w)
-- [let ]xxx = expr / expr;
compileExprTyped fnSigs env expected (Div l r) =
  compileArith fnSigs env expected l r (\sg w -> [if sg == Unsigned then IDivU w else IDiv w])
-- [let ]xxx = -expr;
compileExprTyped fnSigs env expected (Neg e) =
  case intRepr expected of
    Nothing -> Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
    -- 符号なし整数の符号反転は意味を持たないため不可（Rustと同じ。`0u32 - x` のように明示的に書く）
    Just (Unsigned, _) -> Left ("cannot apply unary minus to unsigned type " ++ typeName expected)
    Just (Signed, w) -> do
      ei <- compileExprTyped fnSigs env expected e
      Right (ei ++ [INeg w])
-- [let ]xxx = !expr;
compileExprTyped fnSigs env TBool (Not e) = do
  ei <- compileExprTyped fnSigs env TBool e
  Right (ei ++ [INot])
compileExprTyped _ _ expected (Not _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
-- [let ]xxx = expr == expr;（被演算子の型は expected とは独立に推論する。ポインタ同士の比較も許可する）
compileExprTyped fnSigs env expected (Eq l r) = compileCompare fnSigs env expected l r True (const ICmpEq)
-- [let ]xxx = expr != expr;
compileExprTyped fnSigs env expected (Neq l r) = compileCompare fnSigs env expected l r True (const ICmpNe)
-- [let ]xxx = expr < expr;（大小比較の結果は常にbool。被演算子は整数/ポインタいずれも可、bool同士の比較のみ不可。
-- 符号なし整数は U 版の比較命令を使う）
compileExprTyped fnSigs env expected (Lt l r) = compileCompare fnSigs env expected l r False (\t -> cmpInstr t ICmpLt ICmpLtU)
-- [let ]xxx = expr <= expr;
compileExprTyped fnSigs env expected (Le l r) = compileCompare fnSigs env expected l r False (\t -> cmpInstr t ICmpLe ICmpLeU)
-- [let ]xxx = expr > expr;
compileExprTyped fnSigs env expected (Gt l r) = compileCompare fnSigs env expected l r False (\t -> cmpInstr t ICmpGt ICmpGtU)
-- [let ]xxx = expr >= expr;
compileExprTyped fnSigs env expected (Ge l r) = compileCompare fnSigs env expected l r False (\t -> cmpInstr t ICmpGe ICmpGeU)
-- [let ]xxx = expr as i64; / expr as usize; など（結果は常にキャスト先の型で、expectedと厳密に一致しなければならない。
-- 被演算子はexpectedとは独立に自身の推論型でコンパイルし、型なしリテラルのみの場合はi64とみなす）
compileExprTyped fnSigs env expected (Cast e dstTy)
  | expected /= dstTy =
      Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName dstTy)
  | otherwise = do
      srcTy <- maybe (TyInt Signed W64) id <$> inferMaybeType fnSigs env e
      case (intRepr srcTy, intRepr dstTy) of
        (Just (srcSg, srcW), Just (dstSg, dstW)) -> do
          ei <- compileExprTyped fnSigs env srcTy e
          Right (ei ++ castInstrs srcSg srcW dstSg dstW)
        _ -> Left ("invalid cast: cannot cast " ++ typeName srcTy ++ " to " ++ typeName dstTy)
-- [let ]xxx = f(expr, ...);（引数は宣言順にシグネチャの型で厳密検査し、暗黙変換は行わない）
-- main は戻り値を表示して終了する特別な関数のため、呼び出し元へは戻れない（呼び出し不可）
compileExprTyped _ _ _ (Call "main" _) = Left "cannot call main"
compileExprTyped fnSigs env expected (Call name args) =
  case Map.lookup name fnSigs of
    Nothing -> Left ("undeclared function: " ++ name)
    Just (paramTys, retTy)
      | retTy /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName retTy)
      | length paramTys /= length args ->
          Left
            ( "wrong number of arguments for "
                ++ name
                ++ ": expected "
                ++ show (length paramTys)
                ++ ", found "
                ++ show (length args)
            )
      | otherwise -> do
          argInstrs <- zipWithM (compileExprTyped fnSigs env) paramTys args
          Right (concat argInstrs ++ [ICall name (length args)])
-- [let ]xxx = &lvalue;（lvalueの実効アドレスを計算する。expectedはlvalue自身の型をTPtrで包んだものでなければならない）
compileExprTyped fnSigs env expected (AddrOf e) = do
  (pointeeTy, instrs) <- addressOf fnSigs env e
  if expected == TPtr pointeeTy
    then Right instrs
    else Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName (TPtr pointeeTy))
-- [let ]xxx = *ptr;（expectedをそのまま子へ TPtr expected として伝播する。Castと異なり一様伝播を崩さない）
compileExprTyped _ _ (TArray _ _) (Deref _) = Left "array cannot be used as a value: dereferenced array pointer"
compileExprTyped fnSigs env expected (Deref e) = do
  ei <- compileExprTyped fnSigs env (TPtr expected) e
  Right (ei ++ [LoadInd (storageWidth expected)] ++ loadFixup expected)
-- [let ]xxx = a[i];（要素の実効アドレスを計算し、要素型の幅でロードする。要素型はexpectedと厳密に一致しなければならない）
compileExprTyped fnSigs env expected (Index base idx) = do
  (elemTy, addrInstrs) <- indexAddr fnSigs env base idx
  if elemTy == expected
    then Right (addrInstrs ++ [LoadInd (storageWidth elemTy)] ++ loadFixup elemTy)
    else Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName elemTy)
-- 配列リテラルは let の初期化式（compileArrayInit）でのみ使える
compileExprTyped _ _ _ (ArrayLit _) = Left "array literal can only be used to initialize a let variable"
compileExprTyped _ _ _ (ArrayRepeat _ _) = Left "array literal can only be used to initialize a let variable"

-- 整数型（usize を含む）かどうか
isIntType :: Type -> Bool
isIntType ty = maybe False (const True) (intRepr ty)

-- 二項算術演算の共通処理（expected は整数型でなければならず、両辺に一様伝播させる）
compileArith :: FnSigs -> Env -> Type -> Expr -> Expr -> (Signedness -> Width -> [Instr]) -> Either String [Instr]
compileArith fnSigs env expected l r opInstrs =
  case intRepr expected of
    Nothing -> Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
    Just (sg, w) -> do
      li <- compileExprTyped fnSigs env expected l
      ri <- compileExprTyped fnSigs env expected r
      Right (li ++ ri ++ opInstrs sg w)

-- 比較演算の共通処理（結果は常にbool。被演算子の型は expected とは独立に推論する）。
-- allowBool が False（大小比較）なら bool 同士・() 同士の比較を不可とする
compileCompare :: FnSigs -> Env -> Type -> Expr -> Expr -> Bool -> (Type -> Instr) -> Either String [Instr]
compileCompare fnSigs env expected l r allowBool cmp
  | expected /= TBool = Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
  | otherwise = do
      opTy <- operandType fnSigs env l r
      case opTy of
        TBool | not allowBool -> Left "type mismatch: expected i32 or i64, found bool"
        TUnit | not allowBool -> Left "type mismatch: expected i32 or i64, found ()"
        _ -> do
          li <- compileExprTyped fnSigs env opTy l
          ri <- compileExprTyped fnSigs env opTy r
          Right (li ++ ri ++ [cmp opTy])

-- 配列要素 base[idx] の実効アドレスを計算する命令列と、その要素型を返す（読み出し・代入・&a[i] で共用）。
-- 添字は usize 型で、実行時に範囲検査（IBoundsCheck）を行う。要素数は型から得られるため、
-- 配列へのポインタ経由でも検査できる。添字が整数リテラルであればコンパイル時にも範囲検査する
indexAddr :: FnSigs -> Env -> Expr -> Expr -> Either String (Type, [Instr])
indexAddr fnSigs env base idx = do
  (elemTy, len, baseInstrs) <- arrayBase fnSigs env base
  case literalIndex idx of
    Just n
      | toU64 n >= fromIntegral len ->
          Left ("index out of bounds: the length is " ++ show len ++ " but the index is " ++ show (toU64 n))
    _ -> Right ()
  idxInstrs <- compileExprTyped fnSigs env TUsize idx
  let elemSize = widthBytes (storageWidth elemTy)
  Right (elemTy, baseInstrs ++ idxInstrs ++ [IBoundsCheck len, Push elemSize, IMul W64, IAdd W64])
  where
    literalIndex (Lit n) = Just n
    literalIndex (LitTyped n TUsize) = Just n
    literalIndex _ = Nothing

-- 添字アクセスの基底となる配列の (要素型, 要素数, 先頭アドレスを積む命令列) を返す。
--   a      （配列のローカル変数）    → LoadAddr で変数自身のアドレス（static の配列なら LoadGlobalAddr）
--   ( *p ) （p: &[T; N]）            → p の値がそのまま先頭アドレス
--   p      （&[T; N] 型の任意の式）  → 自動デリファレンス（式の値がそのまま先頭アドレス）
arrayBase :: FnSigs -> Env -> Expr -> Either String (Type, Int, [Instr])
arrayBase _ env (Var name)
  | Just (LLocal off, TArray elemTy len) <- lookupVar name env = Right (elemTy, len, [LoadAddr off])
  | Just (LStatic sym, TArray elemTy len) <- lookupVar name env = Right (elemTy, len, [LoadGlobalAddr sym])
arrayBase fnSigs env base = do
  mty <- inferMaybeType fnSigs env base
  case (base, mty) of
    (Deref e, Just (TArray elemTy len)) -> do
      instrs <- compileExprTyped fnSigs env (TPtr (TArray elemTy len)) e
      Right (elemTy, len, instrs)
    (_, Just ty@(TPtr (TArray elemTy len))) -> do
      instrs <- compileExprTyped fnSigs env ty base
      Right (elemTy, len, instrs)
    (_, Just ty) -> Left ("cannot index into a value of type " ++ typeName ty)
    (_, Nothing) -> Left "cannot index into a value of type integer literal"

-- 配列型の let の初期化。要素ごとに静的なオフセット（先頭 off から要素サイズずつ）へ直接ストアする。
-- [e; N] は e を1回だけ評価して要素0へストアし、残りの要素へはそれをコピーする（副作用は1回のみ）
compileArrayInit :: FnSigs -> Env -> Type -> Int -> Int -> Expr -> Either String [Instr]
compileArrayInit fnSigs env elemTy len off initExpr =
  case initExpr of
    ArrayLit es
      | length es /= len -> lengthMismatch (length es)
      | otherwise ->
          concat
            <$> zipWithM
              (\i e -> (++ [Store w (elemOff i)]) <$> compileExprTyped fnSigs env elemTy e)
              [0 ..]
              es
    ArrayRepeat e k
      | k /= len -> lengthMismatch k
      | otherwise -> do
          ei <- compileExprTyped fnSigs env elemTy e
          Right (ei ++ [Store w (elemOff 0)] ++ concat [[Load w (elemOff 0), Store w (elemOff i)] | i <- [1 .. len - 1]])
    _ -> Left "array must be initialized with an array literal"
  where
    w = storageWidth elemTy
    elemOff i = off + i * widthBytes w
    lengthMismatch found =
      Left ("array length mismatch: expected " ++ show len ++ ", found " ++ show found)

-- 整数キャストの変換命令（Rustの as と同じビットの再解釈）。
-- 32bitへの変換はキャスト先の符号で正規化する（縮小＝下位32bitの切り詰め、同幅の符号変更＝ビットの再解釈）。
-- 32bitから64bitへの拡大はキャスト元の符号で拡張する（i32 は符号拡張、u32 はゼロ拡張）。
-- 64bit同士（i64/u64/usize間の再解釈を含む）は命令不要。
castInstrs :: Signedness -> Width -> Signedness -> Width -> [Instr]
castInstrs _ _ dstSg W32 = [normalize32 dstSg]
castInstrs srcSg W32 _ W64 = [normalize32 srcSg]
castInstrs _ W64 _ W64 = []

-- Virtual machine

-- テストと、const/static の初期値（定数式）の評価で使用する
run :: [Instr] -> Either String Int
run instrs = go instrs [] Map.empty
  where
    go [] [v] _ = Right v
    go [] _ _ = Left "invalid stack state after execution"
    go (Push n : rest) stack vars = go rest (n : stack) vars
    go (IAdd w : rest) (b : a : stack) vars = go rest (trunc w (a + b) : stack) vars
    go (ISub w : rest) (b : a : stack) vars = go rest (trunc w (a - b) : stack) vars
    go (IMul w : rest) (b : a : stack) vars = go rest (trunc w (a * b) : stack) vars
    go (IDiv w : rest) (b : a : stack) vars
      | b == 0 = Left "division by zero"
      | b == -1 = go rest (trunc w (negate a) : stack) vars
      | otherwise = go rest (trunc w (a `quot` b) : stack) vars
    go (IDivU w : rest) (b : a : stack) vars
      | b == 0 = Left "division by zero"
      | otherwise = go rest (divU w a b : stack) vars
    go (INeg w : rest) (a : stack) vars = go rest (trunc w (negate a) : stack) vars
    go (Load w off : rest) stack vars =
      case Map.lookup off vars of
        Just v -> go rest (trunc w v : stack) vars
        Nothing -> Left "uninitialized variable"
    go (Store w off : rest) (v : stack) vars = go rest stack (Map.insert off (trunc w v) vars)
    go (ICmpEq : rest) (b : a : stack) vars = go rest ((if a == b then 1 else 0) : stack) vars
    go (ICmpNe : rest) (b : a : stack) vars = go rest ((if a /= b then 1 else 0) : stack) vars
    go (ICmpLt : rest) (b : a : stack) vars = go rest ((if a < b then 1 else 0) : stack) vars
    go (ICmpLe : rest) (b : a : stack) vars = go rest ((if a <= b then 1 else 0) : stack) vars
    go (ICmpGt : rest) (b : a : stack) vars = go rest ((if a > b then 1 else 0) : stack) vars
    go (ICmpGe : rest) (b : a : stack) vars = go rest ((if a >= b then 1 else 0) : stack) vars
    go (ICmpLtU : rest) (b : a : stack) vars = go rest ((if toU64 a < toU64 b then 1 else 0) : stack) vars
    go (ICmpLeU : rest) (b : a : stack) vars = go rest ((if toU64 a <= toU64 b then 1 else 0) : stack) vars
    go (ICmpGtU : rest) (b : a : stack) vars = go rest ((if toU64 a > toU64 b then 1 else 0) : stack) vars
    go (ICmpGeU : rest) (b : a : stack) vars = go rest ((if toU64 a >= toU64 b then 1 else 0) : stack) vars
    go (INot : rest) (a : stack) vars = go rest ((if a == 0 then 1 else 0) : stack) vars
    go (ISext32 : rest) (a : stack) vars = go rest (trunc W32 a : stack) vars
    go (IZext32 : rest) (a : stack) vars = go rest (zext32 a : stack) vars
    -- vars はオフセットをキーとする抽象メモリなので、"アドレス"はオフセット値そのもの（%rbp=0とみなす）として表現する
    go (LoadAddr off : rest) stack vars = go rest (off : stack) vars
    go (LoadInd w : rest) (addr : stack) vars =
      case Map.lookup addr vars of
        Just v -> go rest (trunc w v : stack) vars
        Nothing -> Left "uninitialized variable"
    go (IBoundsCheck n : rest) stack@(i : _) vars
      | toU64 i >= fromIntegral n = Left "index out of bounds"
      | otherwise = go rest stack vars
    go (StoreInd w : rest) (v : addr : stack) vars = go rest stack (Map.insert addr (trunc w v) vars)
    go (IPop : rest) (_ : stack) vars = go rest stack vars
    -- ラベルは何もしない（main を含む関数の命令列は末尾に関数末尾ラベルを持つ）
    go (Label _ : rest) stack vars = go rest stack vars
    go _ _ _ = Left "stack underflow"

-- 実アセンブリの movl/movslq 等が行う切り詰め・符号拡張を再現する
trunc :: Width -> Int -> Int
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)

-- movl %eax, %eax が行うゼロ拡張を再現する
zext32 :: Int -> Int
zext32 n = fromIntegral (fromIntegral n :: Word32)

-- Int のビットパターンを符号なし64bit整数として解釈する
toU64 :: Int -> Word64
toU64 = fromIntegral

-- divl/divq による符号なし除算を再現する（結果は u32 ならゼロ拡張済みの値になる）
divU :: Width -> Int -> Int -> Int
divU W32 a b = fromIntegral ((fromIntegral a :: Word32) `quot` (fromIntegral b :: Word32))
divU W64 a b = fromIntegral (toU64 a `quot` toU64 b)
