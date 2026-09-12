module Compiler (Instr (..), compile, run) where

import Control.Monad (foldM, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
import Parser (Expr (..), FnDecl (..), Program, Stmt (..), Type (..), Width (..))

-- -- Debug Printサンプル（pTraceShow: 純粋関数内, pTraceShowM: モナド内）
import Debug.Pretty.Simple (pTraceShow, pTraceShowM)

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
  | INeg Width
  | Load Width Int
  | Store Width Int
  | ICmpEq
  | ICmpNe
  | ICmpLt
  | ICmpLe
  | ICmpGt
  | ICmpGe
  | INot
  | -- スタック先頭をpopしてゼロ判定し、真（非ゼロ）ならフォールスルー、偽（ゼロ）ならジャンプする
    JmpIfZero String
  | -- 無条件ジャンプ
    Jmp String
  | -- ジャンプ先ラベルの定義
    Label String
  | -- 下位32bitを符号拡張して64bitへ戻す（to_i64/to_i32 で共通の命令。
    -- to_i64 側は被演算子が既に正規化済みi32であることが前提だが、
    -- リテラル直渡し等で正規化されていない値が漏れないよう、両方向とも同じ命令で強制的に正規化する）
    ISext32
  | -- ローカル変数の%rbp相対実効アドレスを計算しpushする（&lvalue 用）
    LoadAddr Int
  | -- 操作スタック先頭のアドレスをpopし、指定幅でその先の値をロードして
    -- （64bit正規化済みで）push する（*式 用）
    LoadInd Width
  | -- 関数の第N引数レジスタ（SysV AMD64の整数引数レジスタ、0始まり最大6個）の値を
    -- 指定オフセットへストアする（関数プロローグ直後、パラメータをローカル変数と同じ
    -- スタックスロットへスピルするために使う。操作スタックには触れない）
    StoreArg Int Width Int
  | -- 関数呼び出し: 操作スタックに積まれた引数（左から順、個数はIntで指定）を
    -- 引数レジスタへpopしてcallし、戻り値を%raxからpushして戻す
    ICall String Int
  deriving (Show, Eq)

-- Code generator

-- 変数名 -> (%rbp相対オフセット, 型)。スコープのスタック（先頭が最内側）
type Env = [Map String (Int, Type)]

-- 先頭スコープから順に変数を探す（外側のスコープも参照できる）
lookupVar :: String -> Env -> Maybe (Int, Type)
lookupVar _ [] = Nothing
lookupVar name (scope : rest) =
  -- pTraceShow ("name", name) $
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupVar name rest

-- 先頭スコープ（現在のブロック）にのみ宣言されているかを判定する（シャドーイング判定用）
declaredLocally :: String -> Env -> Bool
declaredLocally name (scope : _) = Map.member name scope
declaredLocally _ [] = False

-- 先頭スコープにのみ変数を追加する
insertVar :: String -> (Int, Type) -> Env -> Env
insertVar name v (scope : rest) = Map.insert name v scope : rest
insertVar _ _ [] = []

-- 型ごとのスタック占有バイト数（物理格納幅）
widthBytes :: Width -> Int
widthBytes W32 = 4
widthBytes W64 = 8

-- 型の物理格納幅（bool は i32 用の32bitスロットを流用する。ポインタは指す先の型によらず常に8バイト）
storageWidth :: Type -> Width
storageWidth (TyInt w) = w
storageWidth TBool = W32
storageWidth (TPtr _) = W64

-- エラーメッセージ表示用の型名
typeName :: Type -> String
typeName (TyInt W32) = "i32"
typeName (TyInt W64) = "i64"
typeName TBool = "bool"
typeName (TPtr t) = "&" ++ typeName t

-- 関数シグネチャ表: 名前 -> (仮引数の型リスト, 戻り値の型)。
-- 全fnの本体をコンパイルする前に一括構築する、コンパイル中不変のグローバルな読み取り専用テーブル
-- （宣言順に依存しない相互再帰を可能にするための二パスコンパイルの核）。
type FnSigs = Map String ([Type], Type)

-- SysV AMD64の整数引数レジスタはレジスタ渡し分の最大6個までしか無いため、
-- スタック経由の引数渡し（今回のスコープ外）を避けるべく上限として採用する
maxParams :: Int
maxParams = 6

-- [FnDecl] ＋ [文]＋式（Program）から命令を抽出する。
-- 各fnの本体は外側（暗黙main・他の関数）を一切参照しない独立スコープでコンパイルされ、
-- ラベル採番用のカウンタ（CompileM）は全fn＋暗黙mainを通じて単一のものを共有する
-- （同名ラベルの重複はアセンブル時に壊れるため）。
compile :: Program -> Either String ([(String, [Instr])], Type, [Instr])
compile (fnDecls, stmts, expr) = do
  fnSigs <- resolveFnSigs fnDecls stmts expr
  evalStateT (compileProgram fnSigs fnDecls stmts expr) 0

compileProgram :: FnSigs -> [FnDecl] -> [Stmt] -> Expr -> CompileM ([(String, [Instr])], Type, [Instr])
compileProgram fnSigs fnDecls stmts expr = do
  fns <- mapM (compileFnDecl fnSigs) fnDecls
  -- 暗黙main本体: 外側の関数を持たない（ReturnCtx = Nothing、returnはコンパイルエラー）
  (env, _cursor, stmtInstrs) <- compileStmtsFrom fnSigs [Map.empty] 0 Nothing Nothing stmts
  finalType <- lift (inferType fnSigs env expr)
  exprInstrs <- lift (compileExprTyped fnSigs env finalType expr)
  -- pTraceShowM ("env", env)
  pure (fns, finalType, stmtInstrs ++ exprInstrs)

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

-- 現在コンパイル中の関数の (戻り値の型, 関数末尾ラベル)。関数の外側（暗黙main）では Nothing であり、
-- returnの使用はコンパイルエラーとなる。LoopCtxと同じ理由で普通の関数引数として渡す：
-- ブロック/if分岐/while本体へは変更せずそのまま素通しし（returnは常に直近の外側“関数”から戻るため、
-- ループを跨いでも値を保ち続ける必要がある）、関数本体に入るときだけ新しい値に差し替える。
type ReturnCtx = Maybe (Type, String)

-- 任意のEnv/cursor/loopCtx/returnCtxを起点に[文]から命令を抽出する（ブロック/if分岐/while本体/関数本体の再帰コンパイルに使う）
compileStmtsFrom :: FnSigs -> Env -> Int -> LoopCtx -> ReturnCtx -> [Stmt] -> CompileM (Env, Int, [Instr])
compileStmtsFrom fnSigs initEnv initCursor loopCtx returnCtx stmts = foldM step (initEnv, initCursor, []) stmts
 where
  -- let xxx: 型 = ...
  step (env, cursor, acc) (SLet name ty expr) = do
    -- 変数の二重定義をチェック（同一ブロック内の再宣言のみ対象。外側との同名はシャドーイングとして許可）
    when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
    -- 式の表現から命令を抽出（宣言された型を期待型として渡す）
    instrs <- lift (compileExprTyped fnSigs env ty expr)
    -- 新しく登録する変数のスタック上のアドレスを、型の物理格納幅分だけ詰めて計算
    let off = cursor - widthBytes (storageWidth ty)
    -- 変数とアドレスのマップ, 命令＋追加命令＋変数のストア
    pure (insertVar name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
  -- let xxx = ...（型注釈省略。inferTypeで推論し、最後まで未確定ならi64をデフォルトとする）
  step (env, cursor, acc) (SLetInferred name expr) = do
    when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
    ty <- lift (inferType fnSigs env expr)
    instrs <- lift (compileExprTyped fnSigs env ty expr)
    let off = cursor - widthBytes (storageWidth ty)
    pure (insertVar name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
  -- xxx = ...
  step (env, cursor, acc) (SAssign name expr) = do
    -- 変数の定義をチェック（外側スコープの変数への書き込みも許可）
    (off, ty) <- lift (maybe (Left ("undeclared variable: " ++ name)) Right (lookupVar name env))
    -- 式の表現から命令を抽出（既存の変数の型を期待型として渡す）
    instrs <- lift (compileExprTyped fnSigs env ty expr)
    -- 変数とアドレスのマップはそのまま, 命令＋追加命令＋変数のストア
    pure (env, cursor, acc ++ instrs ++ [Store (storageWidth ty) off])
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
    (retTy, endLabel) <- lift (maybe (Left "return used outside function") Right returnCtx)
    instrs <- lift (compileExprTyped fnSigs env retTy expr)
    pure (env, cursor, acc ++ instrs ++ [Jmp endLabel])

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
allocParams :: [(String, Type)] -> ([(String, Int, Type)], Map String (Int, Type), Int)
allocParams params = (reverse revAssigned, Map.fromList [(n, (o, t)) | (n, o, t) <- revAssigned], cursor)
 where
  (revAssigned, cursor) = foldl step ([], 0) params
  step (acc, c) (name, ty) =
    let off = c - widthBytes (storageWidth ty)
     in ((name, off, ty) : acc, off)

-- 関数本体のコンパイル。パラメータのみを含む独立スコープ（外側の暗黙main・他の関数の変数は
-- 一切参照できない）から開始し、パラメータはプロローグ直後にレジスタからスタックへスピルする
-- （StoreArg）。早期returnと末尾式のフォールスルーは同じ関数末尾ラベルへ合流する
compileFnDecl :: FnSigs -> FnDecl -> CompileM (String, [Instr])
compileFnDecl fnSigs (FnDecl name params _ (stmts, tailExpr)) = do
  endLabel <- freshLabel "fn_end"
  -- パラメータ・戻り値の具体的な型は（型注釈が省略されていた場合を含め）resolveFnSigsが
  -- 確定させたfnSigsを常に正とする（FnDecl自身のフィールドはMaybe Typeであり得るため使わない）
  let (paramTys, retTy) = fnSigs Map.! name
      (assigned, paramEnv, paramCursor) = allocParams (zip (map fst params) paramTys)
      env0 = [paramEnv]
      spillInstrs = [StoreArg i (storageWidth ty) off | (i, (_, off, ty)) <- zip [0 ..] assigned]
      returnCtx = Just (retTy, endLabel)
  (env1, _cursor1, stmtInstrs) <- compileStmtsFrom fnSigs env0 paramCursor Nothing returnCtx stmts
  tailInstrs <- lift (compileExprTyped fnSigs env1 retTy tailExpr)
  -- pTraceShowM ("assigned of " ++ name, assigned)
  -- pTraceShowM ("env0 of " ++ name, env0)
  -- pTraceShowM ("env1 of " ++ name, env1)
  pure (name, spillInstrs ++ stmtInstrs ++ tailInstrs ++ [Label endLabel])

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
inferMaybeType fnSigs env = go
 where
  go (Lit _) = Right Nothing
  -- サフィックス付きリテラルは Var と同様、常に確定した型を持つ（expectedとは無関係）
  go (LitTyped _ w) = Right (Just (TyInt w))
  go (BoolLit _) = Right (Just TBool)
  go (Var name) =
    maybe (Left ("undeclared variable: " ++ name)) (Right . Just . snd) (lookupVar name env)
  go (Neg e) = go e
  go (Not e) = do
    t <- go e
    case t of
      Just (TyInt w) -> Left ("type mismatch: expected bool, found " ++ typeName (TyInt w))
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
  -- to_i64/to_i32 の結果型は被演算子によらず常に確定する（BoolLit と同様、部分木を辿る必要はない）
  go (ToI64 _) = Right (Just (TyInt W64))
  go (ToI32 _) = Right (Just (TyInt W32))
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
inferType fnSigs env expr = maybe (TyInt W64) id <$> inferMaybeType fnSigs env expr

-- Eq/Neq の被演算子同士の共通の型を決定する（外側の expected とは独立に推論する）
operandType :: FnSigs -> Env -> Expr -> Expr -> Either String Type
operandType fnSigs env a b = do
  ta <- inferMaybeType fnSigs env a
  tb <- inferMaybeType fnSigs env b
  maybe (TyInt W64) id <$> unifyMaybeType ta tb

-- lvalue（&の対象になれる式）の実効アドレスを計算する命令列と、その参照先の型を返す。
-- lvalueとして認めるのは変数参照と、ポインタのデリファレンス（&*p は p 自身の値が
-- 既にアドレスそのものなので LoadAddr/LoadInd を使わず p を普通にコンパイルするだけでよい）の2種のみ。
-- それ以外（リテラル・算術式・関数呼び出し・&式自身 等）は意味論エラーとする。
addressOf :: FnSigs -> Env -> Expr -> Either String (Type, [Instr])
addressOf _ env (Var name) =
  maybe
    (Left ("undeclared variable: " ++ name))
    (\(off, ty) -> Right (ty, [LoadAddr off]))
    (lookupVar name env)
addressOf fnSigs env (Deref e) = do
  t <-
    inferMaybeType fnSigs env e
      >>= maybe (Left "type mismatch: cannot take address of a dereferenced untyped literal") Right
  case t of
    TPtr inner -> (,) inner <$> compileExprTyped fnSigs env t e
    other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
addressOf _ _ e = Left ("invalid operand for &: not an lvalue: " ++ show e)

-- fnの仮引数・戻り値型の注釈省略と型推論（FnSigs確定前の1パス目）
--
-- letと異なりパラメータには初期化式が無いため、型の根拠は
-- (a) 戻り値型なら自身のreturn文・末尾式、(b) パラメータ型なら呼び出し側の実引数式
-- からしか得られず、複数関数にまたがる依存関係・循環（自己再帰・相互再帰）が生じ得る。
-- 呼び出される関数の本体内でのパラメータの使われ方は一切見ない（ボトムアップ推論はしない）。

-- 型注釈が省略されたシグネチャ要素1個を指す
data Slot = ParamSlot String Int | ReturnSlot String
  deriving (Eq, Ord, Show)

-- 循環検出のエラーメッセージ表示用
describeSlot :: Slot -> String
describeSlot (ParamSlot name i) = name ++ " param#" ++ show (i + 1)
describeSlot (ReturnSlot name) = name ++ " return type"

-- これまでに解決済みのスロットの値
type ResolvedSlots = Map Slot Type

-- 関数本体（または暗黙main）内のローカル変数（仮引数・let）の、型解決の観点からの状態。
-- Env（%rbp相対オフセット, 型）と異なり、まだ解決されていないスロット待ちのことがある
type LocalEnv = [Map String (Either Slot Type)]

lookupLocal :: String -> LocalEnv -> Maybe (Either Slot Type)
lookupLocal _ [] = Nothing
lookupLocal name (scope : rest) = case Map.lookup name scope of
  Just v -> Just v
  Nothing -> lookupLocal name rest

setLocal :: String -> Either Slot Type -> LocalEnv -> LocalEnv
setLocal name v (scope : rest) = Map.insert name v scope : rest
setLocal _ _ [] = []

-- 1個の式を型付けしようとした結果:
--   Left err          = 確定的な型エラー（未宣言変数・未宣言関数・bool/ポインタ関連の不整合等）
--   Right (Left slot) = 特定のスロットが解決されるまで結論が出せない
--   Right (Right mty) = 証拠が得られた（Nothing = 未確定の整数リテラルのみ。inferMaybeTypeのNothingと同じ意味）
type SlotResult = Either String (Either Slot (Maybe Type))

-- SlotResultを一段展開し、ブロックされていれば即座に伝播し、値が取れていれば継続関数へ渡す
chainSlot :: SlotResult -> (Maybe Type -> SlotResult) -> SlotResult
chainSlot r k = do
  v <- r
  case v of
    Left slot -> Right (Left slot)
    Right ty -> k ty

-- lvalue（&の対象）のアドレス先の型を、LocalEnv上で解決する（addressOfのLocalEnv版）
addressOfSlot :: Map String FnDecl -> ResolvedSlots -> LocalEnv -> Expr -> Either String (Either Slot Type)
addressOfSlot _ _ env (Var name) =
  maybe (Left ("undeclared variable: " ++ name)) Right (lookupLocal name env)
addressOfSlot fnDeclMap resolved env (Deref e) = do
  r <- resolveExprType fnDeclMap resolved env e
  case r of
    Left slot -> Right (Left slot)
    Right (Just (TPtr inner)) -> Right (Right inner)
    Right (Just other) -> Left ("type mismatch: expected pointer, found " ++ typeName other)
    Right Nothing -> Left "type mismatch: cannot take address of a dereferenced untyped literal"
addressOfSlot _ _ _ e = Left ("invalid operand for &: not an lvalue: " ++ show e)

-- inferMaybeTypeのLocalEnv版。Var/CallがまだResolvedSlotsに無いスロットを指していれば
-- そのスロットへブロックし、それ以外の構造はinferMaybeTypeと完全に同一のロジックで型を求める
resolveExprType :: Map String FnDecl -> ResolvedSlots -> LocalEnv -> Expr -> SlotResult
resolveExprType fnDeclMap resolved env = go
 where
  go (Lit _) = Right (Right Nothing)
  go (LitTyped _ w) = Right (Right (Just (TyInt w)))
  go (BoolLit _) = Right (Right (Just TBool))
  go (Var name) = case lookupLocal name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (Left slot) -> Right (Left slot)
    Just (Right ty) -> Right (Right (Just ty))
  go (Neg e) = go e
  go (Not e) =
    go e `chainSlot` \t -> case t of
      Just (TyInt w) -> Left ("type mismatch: expected bool, found " ++ typeName (TyInt w))
      _ -> Right (Right (Just TBool))
  go (Add a b) = combine a b
  go (Sub a b) = combine a b
  go (Mul a b) = combine a b
  go (Div a b) = combine a b
  go (Eq a b) = combineBool a b
  go (Neq a b) = combineBool a b
  go (Lt a b) = combineBool a b
  go (Le a b) = combineBool a b
  go (Gt a b) = combineBool a b
  go (Ge a b) = combineBool a b
  go (ToI64 _) = Right (Right (Just (TyInt W64)))
  go (ToI32 _) = Right (Right (Just (TyInt W32)))
  go (AddrOf e) = case addressOfSlot fnDeclMap resolved env e of
    Left err -> Left err
    Right (Left slot) -> Right (Left slot)
    Right (Right ty) -> Right (Right (Just (TPtr ty)))
  go (Deref e) =
    go e `chainSlot` \t -> case t of
      Just (TPtr inner) -> Right (Right (Just inner))
      Just other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
      Nothing -> Left "type mismatch: cannot dereference an untyped literal"
  go (Call name _) = case Map.lookup name fnDeclMap of
    Nothing -> Left ("undeclared function: " ++ name)
    Just _ -> case Map.lookup (ReturnSlot name) resolved of
      Just ty -> Right (Right (Just ty))
      Nothing -> Right (Left (ReturnSlot name))
  combine a b =
    go a `chainSlot` \ta ->
      go b `chainSlot` \tb ->
        case unifyMaybeType ta tb of
          Left err -> Left err
          Right u -> Right (Right u)
  combineBool a b = combine a b `chainSlot` \_ -> Right (Right (Just TBool))

-- (対象スロット, その証拠) のリスト。プログラム全体を1回走査してまとめて集める
type Evidence = [(Slot, Either Slot (Maybe Type))]

-- 式ツリー中のあらゆる位置に現れるCallノードを見つけ、各実引数式についてParamSlotへの証拠を集める
-- （宣言済みの仮引数の個数を超える位置はスキップする。実際の引数個数不一致はFnSigs確定後に検出される）
callEvidence :: Map String FnDecl -> ResolvedSlots -> LocalEnv -> Expr -> Either String Evidence
callEvidence fnDeclMap resolved env = go
 where
  go (Lit _) = Right []
  go (LitTyped _ _) = Right []
  go (BoolLit _) = Right []
  go (Var _) = Right []
  go (Neg e) = go e
  go (Not e) = go e
  go (Add a b) = combine a b
  go (Sub a b) = combine a b
  go (Mul a b) = combine a b
  go (Div a b) = combine a b
  go (Eq a b) = combine a b
  go (Neq a b) = combine a b
  go (Lt a b) = combine a b
  go (Le a b) = combine a b
  go (Gt a b) = combine a b
  go (Ge a b) = combine a b
  go (ToI64 e) = go e
  go (ToI32 e) = go e
  go (AddrOf e) = go e
  go (Deref e) = go e
  go (Call name args) = do
    nested <- concat <$> mapM go args -- 再帰的に型情報を取得（ネストした型情報）
    pTraceShowM ("nested", nested)
    paramEv <- -- 既知の型情報(fnDeclMap,resolved,env)から取得
      mapM
        (\(i, arg) -> (,) (ParamSlot name i) <$> resolveExprType fnDeclMap resolved env arg)
        (zip [0 ..] args)
    pTraceShowM ("paramEv", paramEv)
    pure (nested ++ paramEv) -- 再帰的に取得 ＋＋ 既知の情報から取得
  combine a b = (++) <$> go a <*> go b

-- 関数本体（または暗黙main）内の文列を辿り、(対象スロット, 証拠) を集める。compileStmtsFrom と同じ形
-- （スコープのpush/pop、SBlock/SIf/SWhileの再帰）だが、命令列の代わりに証拠を集める点だけが異なる
collectEvidenceStmts ::
  Map String FnDecl -> ResolvedSlots -> Maybe String -> LocalEnv -> [Stmt] -> Either String (LocalEnv, Evidence)
collectEvidenceStmts fnDeclMap resolved curFn = do
  pTraceShowM ("collectEvidenceStmts実行", curFn)
  go
 where
  go env [] = Right (env, [])
  go env (stmt : rest) = do
    (env', ev1) <- step env stmt
    (env'', ev2) <- go env' rest
    pure (env'', ev1 ++ ev2)

  step env (SLet name ty expr) = do
    ev <- callEvidence fnDeclMap resolved env expr
    -- pTraceShowM ("ev", ev)
    pure (setLocal name (Right ty) env, ev)
  step env (SLetInferred name expr) = do
    ev <- callEvidence fnDeclMap resolved env expr
    -- pTraceShowM ("ev", ev)
    status <- case resolveExprType fnDeclMap resolved env expr of
      Left err -> Left err
      Right (Left slot) -> Right (Left slot)
      Right (Right (Just ty)) -> Right (Right ty)
      Right (Right Nothing) -> Right (Right (TyInt W64))
    pure (setLocal name status env, ev)
  step env (SAssign _ expr) = do
    ev <- callEvidence fnDeclMap resolved env expr
    -- pTraceShowM ("ev", ev)
    pure (env, ev)
  step env (SBlock inner) = do
    (_, ev) <- collectEvidenceStmts fnDeclMap resolved curFn (Map.empty : env) inner
    pure (env, ev)
  step env (SIf branches maybeElse) = do
    branchEv <-
      concat
        <$> mapM
          ( \(cond, body) -> do
              condEv <- callEvidence fnDeclMap resolved env cond
              -- pTraceShowM ("condEv", condEv)
              (_, bodyEv) <- collectEvidenceStmts fnDeclMap resolved curFn (Map.empty : env) body
              pure (condEv ++ bodyEv)
          )
          branches
    elseEv <- case maybeElse of
      Nothing -> Right []
      Just elseStmts -> snd <$> collectEvidenceStmts fnDeclMap resolved curFn (Map.empty : env) elseStmts
    pure (env, branchEv ++ elseEv)
  step env (SWhile cond body) = do
    condEv <- callEvidence fnDeclMap resolved env cond
    -- pTraceShowM ("condEv", condEv)
    (_, bodyEv) <- collectEvidenceStmts fnDeclMap resolved curFn (Map.empty : env) body
    pure (env, condEv ++ bodyEv)
  step env SBreak = Right (env, [])
  step env SContinue = Right (env, [])
  step env (SReturn expr) = do
    ev <- callEvidence fnDeclMap resolved env expr
    -- pTraceShowM ("ev", ev)
    case curFn of
      Nothing -> pure (env, ev)
      Just fnName -> do
        r <- resolveExprType fnDeclMap resolved env expr
        pure (env, ev ++ [(ReturnSlot fnName, r)])

-- 仮引数をLocalEnvの初期スコープへ変換する（型注釈済みならその型、省略済みならこれまでの解決状況を反映する）
paramLocalScope :: ResolvedSlots -> String -> [(String, Maybe Type)] -> Map String (Either Slot Type)
paramLocalScope resolved fnName params =
  Map.fromList
    [ (name, status)
    | (i, (name, mty)) <- zip [0 ..] params
    , let slot = ParamSlot fnName i
          status = case mty of
            Just ty -> Right ty
            Nothing -> maybe (Left slot) Right (Map.lookup slot resolved)
    ]

-- プログラム全体（全fn本体＋暗黙main）を1回走査し、現在のResolvedSlotsに対する証拠を集める
programEvidence :: Map String FnDecl -> ResolvedSlots -> [FnDecl] -> [Stmt] -> Expr -> Either String Evidence
programEvidence fnDeclMap resolved fnDecls stmts tailExpr = do
  pTraceShowM ("programEvidence実行", tailExpr)
  fnEv <- concat <$> mapM (fnDeclEvidence fnDeclMap resolved) fnDecls -- 全ての関数から証拠を取得
  (env1, topEv) <- collectEvidenceStmts fnDeclMap resolved Nothing [Map.empty] stmts -- 暗黙mainの文からLocalEnv、証拠を取得
  tailEv <- callEvidence fnDeclMap resolved env1 tailExpr -- 暗黙mainの末尾式から証拠を取得
  pTraceShowM ("fnEv", fnEv)
  pTraceShowM ("env1", env1)
  pTraceShowM ("tailEv", tailEv)
  pure (fnEv ++ topEv ++ tailEv)

-- １つの関数に対して仮引数、本文、末尾式から証拠を集める
fnDeclEvidence :: Map String FnDecl -> ResolvedSlots -> FnDecl -> Either String [(Slot, Either Slot (Maybe Type))]
fnDeclEvidence fnDeclMap resolved (FnDecl name params _ (body, tailE)) = do
  pTraceShowM ("fnDeclEvidence実行", name)
  let env0 = [paramLocalScope resolved name params] -- 仮引数をresolvedから検索してLocalEnvを取得
  (env1, bodyEv) <- collectEvidenceStmts fnDeclMap resolved (Just name) env0 body -- 関数の本文から(LocalEnv、証拠)を取得
  tailCallEv <- callEvidence fnDeclMap resolved env1 tailE -- 末尾式内のCallから実引数の証拠を取得
  tailRet <- resolveExprType fnDeclMap resolved env1 tailE -- 末尾式から戻り値の証拠を取得
  pTraceShowM ("env0", env0)
  pTraceShowM ("env1", env1)
  pTraceShowM ("bodyEv", bodyEv)
  pTraceShowM ("tailCallEv", tailCallEv)
  pTraceShowM ("tailRet", tailRet)
  pure (bodyEv ++ tailCallEv ++ [(ReturnSlot name, tailRet)]) -- 本文、末尾式内の実引数、末尾式の戻り値からの証拠を連結して返却

-- 構造検証: main予約名・重複定義・最大引数数（旧buildFnSigsのチェックをそのまま踏襲する。型の中身は見ない）
validateFnShapes :: [FnDecl] -> Either String ()
validateFnShapes fnDecls = () <$ foldM addSig Map.empty fnDecls
 where
  addSig :: Map String () -> FnDecl -> Either String (Map String ())
  addSig seen (FnDecl name params _ _)
    | name == "main" = Left "function name 'main' is reserved"
    | Map.member name seen = Left ("function already declared: " ++ name)
    | length params > maxParams = Left ("too many parameters (max " ++ show maxParams ++ "): " ++ name)
    | otherwise = Right (Map.insert name () seen)

-- 各fnの型注釈が省略された箇所をSlotとして列挙し、注釈済みの箇所は即座にResolvedSlotsへ投入する
initialSlots :: [FnDecl] -> (ResolvedSlots, [Slot])
initialSlots fnDecls = (Map.fromList resolved, unresolved)
 where
  entries =
    [ (slot, mty)
    | FnDecl name params mret _ <- fnDecls
    , (slot, mty) <- zip (map (ParamSlot name) [0 ..]) (map snd params) ++ [(ReturnSlot name, mret)]
    ]
  resolved = [(slot, ty) | (slot, Just ty) <- entries]
  unresolved = [slot | (slot, Nothing) <- entries]

-- 未解決スロット集合が空になるまで、全プログラムを繰り返し走査して解決を進める。
-- 1個のスロットについて、自分自身待ちの証拠は無視する（通常の自己再帰が誤って循環と
-- 判定されるのを防ぐ）。1ラウンドで1つも前進しなければ、残りは（自己ループ以外の）
-- 未解決スロット同士で行き詰まっている＝循環と判定してエラーとする
resolveSlots ::
  Map String FnDecl -> [FnDecl] -> [Stmt] -> Expr -> ResolvedSlots -> [Slot] -> Either String ResolvedSlots
resolveSlots _ _ _ _ resolved [] = Right resolved -- 未解決スロットが空になったら終了
resolveSlots fnDeclMap fnDecls stmts tailExpr resolved pending = do
  evidence <- programEvidence fnDeclMap resolved fnDecls stmts tailExpr
  results <- mapM (resolveOne evidence) pending
  pTraceShowM ("evidence", evidence)
  pTraceShowM ("results", results)
  let advanced = [(slot, ty) | (slot, Right ty) <- zip pending results]
      stillPending = [slot | (slot, Left _) <- zip pending results]
  if null advanced
    then case [(slot, slot') | (slot, Left slot') <- zip pending results] of
      [] -> Left "circular type inference: unresolvable signature (add an explicit type annotation to break the cycle)"
      ((blockedSlot, blockedOn) : _) ->
        Left
          ( "circular type inference: "
              ++ describeSlot blockedSlot
              ++ " -> "
              ++ describeSlot blockedOn
              ++ " (add an explicit type annotation to break the cycle)"
          )
    else resolveSlots fnDeclMap fnDecls stmts tailExpr (Map.union (Map.fromList advanced) resolved) stillPending

-- Evidenceから対象スロット == slotかつ自分自身待ち（Left slot）ではないものだけをcandidatesとして抜き出す
-- candidatesの中にLeft blocker（他のスロット待ち）が1つでも残っていれば、slotはまだblocker待ちとしてRight (Left blocker)を返す（＝今ラウンドでは前進しない）
-- 残りが全部Right mtyなら、それらをunifyMaybeTypeで1つの型に単一化する。証拠が1つも無ければi64にデフォルトする
resolveOne :: (Eq a) => [(a, Either a (Maybe Type))] -> a -> Either String (Either a Type)
resolveOne evidence slot = do
  let candidates = [c | (s, c) <- evidence, s == slot, c /= Left slot]
  case [s' | Left s' <- candidates] of
    (blocker : _) -> Right (Left blocker)
    [] -> do
      u <- foldM unifyMaybeType Nothing [mty | Right mty <- candidates]
      Right (Right (maybe (TyInt W64) id u))

-- 全fn定義から（型注釈省略を含めて）FnSigsを一括解決する（本体のコンパイルより前に行う1パス目）。
-- 呼び出し規約・スタックスロット割り付けが必要とする具体的なTypeを、注釈または
-- 呼び出し箇所の実引数・自身のreturn/末尾式からの推論のいずれかで確定させる
resolveFnSigs :: [FnDecl] -> [Stmt] -> Expr -> Either String FnSigs
resolveFnSigs fnDecls stmts tailExpr = do
  validateFnShapes fnDecls
  let fnDeclMap = Map.fromList [(name, d) | d@(FnDecl name _ _ _) <- fnDecls]
      (initResolved, pending) = initialSlots fnDecls -- 全ての関数から解決済みスロット、未解決スロットを取得
  resolved <- resolveSlots fnDeclMap fnDecls stmts tailExpr initResolved pending
  -- pTraceShowM ("initResolved", initResolved)
  -- pTraceShowM ("pending", pending)
  -- pTraceShowM ("resolved", resolved)
  let paramType name (i, (_, mty)) = maybe (resolved Map.! ParamSlot name i) id mty
      retType name mty = maybe (resolved Map.! ReturnSlot name) id mty
  pure
    ( Map.fromList
        [ (name, (map (paramType name) (zip [0 ..] params), retType name mret))
        | FnDecl name params mret _ <- fnDecls
        ]
    )

-- 期待する型（expected）を一様に伝播させながら命令を抽出する。
-- Var の実際の型が expected と食い違えば型不一致エラーとする。
-- 算術演算・整数リテラルは TyInt 専用、Not/BoolLit/Eq/Neq は TBool 専用であり、
-- expected と食い違えばその場でエラーとする。
compileExprTyped :: FnSigs -> Env -> Type -> Expr -> Either String [Instr]
-- [let ]xxx = 1234;（リテラルは無型なので expected をそのまま採用する。ただし bool・ポインタは不可）
compileExprTyped _ _ TBool (Lit _) = Left "type mismatch: expected bool, found integer literal"
compileExprTyped _ _ expected@(TPtr _) (Lit _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found integer literal")
compileExprTyped _ _ (TyInt _) (Lit n) = Right [Push n]
-- [let ]xxx = 1234i64; / 1234i32;（サフィックスで型が確定済み。Varと同様、expectedと食い違えばエラー）
compileExprTyped _ _ expected (LitTyped n w)
  | expected == TyInt w = Right [Push n]
  | otherwise = Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName (TyInt w))
-- [let ]xxx = true; / false;（bool以外のコンテキストでは不可）
compileExprTyped _ _ TBool (BoolLit b) = Right [Push (if b then 1 else 0)]
compileExprTyped _ _ expected@(TyInt _) (BoolLit _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (BoolLit _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
-- [let ]xxx = yyy;
compileExprTyped _ env expected (Var name) =
  case lookupVar name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (off, ty)
      | ty /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName ty)
      | otherwise -> Right [Load (storageWidth ty) off]
-- [let ]xxx = expr + expr;
compileExprTyped _ _ TBool (Add _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped _ _ expected@(TPtr _) (Add _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
compileExprTyped fnSigs env expected@(TyInt w) (Add l r) = do
  li <- compileExprTyped fnSigs env expected l
  ri <- compileExprTyped fnSigs env expected r
  Right (li ++ ri ++ [IAdd w])
-- [let ]xxx = expr - expr;
compileExprTyped _ _ TBool (Sub _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped _ _ expected@(TPtr _) (Sub _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
compileExprTyped fnSigs env expected@(TyInt w) (Sub l r) = do
  li <- compileExprTyped fnSigs env expected l
  ri <- compileExprTyped fnSigs env expected r
  Right (li ++ ri ++ [ISub w])
-- [let ]xxx = expr * expr;
compileExprTyped _ _ TBool (Mul _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped _ _ expected@(TPtr _) (Mul _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
compileExprTyped fnSigs env expected@(TyInt w) (Mul l r) = do
  li <- compileExprTyped fnSigs env expected l
  ri <- compileExprTyped fnSigs env expected r
  Right (li ++ ri ++ [IMul w])
-- [let ]xxx = expr / expr;
compileExprTyped _ _ TBool (Div _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped _ _ expected@(TPtr _) (Div _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
compileExprTyped fnSigs env expected@(TyInt w) (Div l r) = do
  li <- compileExprTyped fnSigs env expected l
  ri <- compileExprTyped fnSigs env expected r
  Right (li ++ ri ++ [IDiv w])
-- [let ]xxx = -expr;
compileExprTyped _ _ TBool (Neg _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped _ _ expected@(TPtr _) (Neg _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
compileExprTyped fnSigs env expected@(TyInt w) (Neg e) = do
  ei <- compileExprTyped fnSigs env expected e
  Right (ei ++ [INeg w])
-- [let ]xxx = !expr;
compileExprTyped _ _ expected@(TyInt _) (Not _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Not _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Not e) = do
  ei <- compileExprTyped fnSigs env TBool e
  Right (ei ++ [INot])
-- [let ]xxx = expr == expr;（被演算子の型は expected とは独立に推論する。ポインタ同士の比較も許可する）
compileExprTyped _ _ expected@(TyInt _) (Eq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Eq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Eq l r) = do
  opTy <- operandType fnSigs env l r
  li <- compileExprTyped fnSigs env opTy l
  ri <- compileExprTyped fnSigs env opTy r
  Right (li ++ ri ++ [ICmpEq])
-- [let ]xxx = expr != expr;
compileExprTyped _ _ expected@(TyInt _) (Neq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Neq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Neq l r) = do
  opTy <- operandType fnSigs env l r
  li <- compileExprTyped fnSigs env opTy l
  ri <- compileExprTyped fnSigs env opTy r
  Right (li ++ ri ++ [ICmpNe])
-- [let ]xxx = expr < expr;（大小比較の結果は常にbool。被演算子はi32/i64/ポインタいずれも可、bool同士の比較のみ不可）
compileExprTyped _ _ expected@(TyInt _) (Lt _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Lt _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Lt l r) = do
  opTy <- operandType fnSigs env l r
  case opTy of
    TBool -> Left "type mismatch: expected i32 or i64, found bool"
    _ -> do
      li <- compileExprTyped fnSigs env opTy l
      ri <- compileExprTyped fnSigs env opTy r
      Right (li ++ ri ++ [ICmpLt])
-- [let ]xxx = expr <= expr;
compileExprTyped _ _ expected@(TyInt _) (Le _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Le _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Le l r) = do
  opTy <- operandType fnSigs env l r
  case opTy of
    TBool -> Left "type mismatch: expected i32 or i64, found bool"
    _ -> do
      li <- compileExprTyped fnSigs env opTy l
      ri <- compileExprTyped fnSigs env opTy r
      Right (li ++ ri ++ [ICmpLe])
-- [let ]xxx = expr > expr;
compileExprTyped _ _ expected@(TyInt _) (Gt _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Gt _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Gt l r) = do
  opTy <- operandType fnSigs env l r
  case opTy of
    TBool -> Left "type mismatch: expected i32 or i64, found bool"
    _ -> do
      li <- compileExprTyped fnSigs env opTy l
      ri <- compileExprTyped fnSigs env opTy r
      Right (li ++ ri ++ [ICmpGt])
-- [let ]xxx = expr >= expr;
compileExprTyped _ _ expected@(TyInt _) (Ge _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped _ _ expected@(TPtr _) (Ge _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped fnSigs env TBool (Ge l r) = do
  opTy <- operandType fnSigs env l r
  case opTy of
    TBool -> Left "type mismatch: expected i32 or i64, found bool"
    _ -> do
      li <- compileExprTyped fnSigs env opTy l
      ri <- compileExprTyped fnSigs env opTy r
      Right (li ++ ri ++ [ICmpGe])
-- [let ]xxx = to_i64(expr);（結果は常にi64。被演算子はexpectedとは独立に常にi32を要求する）
compileExprTyped _ _ TBool (ToI64 _) = Left "type mismatch: expected bool, found i64"
compileExprTyped _ _ expected@(TyInt W32) (ToI64 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i64")
compileExprTyped _ _ expected@(TPtr _) (ToI64 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i64")
compileExprTyped fnSigs env (TyInt W64) (ToI64 e) = do
  ei <- compileExprTyped fnSigs env (TyInt W32) e
  Right (ei ++ [ISext32])
-- [let ]xxx = to_i32(expr);（結果は常にi32。被演算子はexpectedとは独立に常にi64を要求する）
compileExprTyped _ _ TBool (ToI32 _) = Left "type mismatch: expected bool, found i32"
compileExprTyped _ _ expected@(TyInt W64) (ToI32 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i32")
compileExprTyped _ _ expected@(TPtr _) (ToI32 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i32")
compileExprTyped fnSigs env (TyInt W32) (ToI32 e) = do
  ei <- compileExprTyped fnSigs env (TyInt W64) e
  Right (ei ++ [ISext32])
-- [let ]xxx = f(expr, ...);（引数は宣言順にシグネチャの型で厳密検査し、暗黙変換は行わない）
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
-- [let ]xxx = *ptr;（expectedをそのまま子へ TPtr expected として伝播する。ToI64/ToI32と異なり一様伝播を崩さない）
compileExprTyped fnSigs env expected (Deref e) = do
  ei <- compileExprTyped fnSigs env (TPtr expected) e
  Right (ei ++ [LoadInd (storageWidth expected)])

-- Virtual machine

-- テストでのみ使用
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
    | otherwise = go rest (trunc w (a `quot` b) : stack) vars
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
  go (INot : rest) (a : stack) vars = go rest ((if a == 0 then 1 else 0) : stack) vars
  go (ISext32 : rest) (a : stack) vars = go rest (trunc W32 a : stack) vars
  -- vars はオフセットをキーとする抽象メモリなので、"アドレス"はオフセット値そのもの（%rbp=0とみなす）として表現する
  go (LoadAddr off : rest) stack vars = go rest (off : stack) vars
  go (LoadInd w : rest) (addr : stack) vars =
    case Map.lookup addr vars of
      Just v -> go rest (trunc w v : stack) vars
      Nothing -> Left "uninitialized variable"
  go _ _ _ = Left "stack underflow"

-- 実アセンブリの movl/movslq 等が行う切り詰め・符号拡張を再現する
trunc :: Width -> Int -> Int
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)
