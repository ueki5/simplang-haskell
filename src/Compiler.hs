module Compiler (Token (..), Expr (..), Stmt (..), Instr (..), Width (..), Type (..), Program, tokenize, parse, compile, run) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map

-- -- Debug Printサンプル（ipTraceShow: 純粋関数内, pTraceShowM: モナド内）
-- import Debug.Pretty.Simple (pTraceShow, pTraceShowM)

-- -- プリティ印刷が不要な場合
-- trace: 純粋関数内(既存の第一引数を表示、第二引数を返却)
-- print/putStrLn: IOモナド内
-- import Debug.Trace (trace)

data Token
  = TInt Int
  | TIdent String
  | TLet
  | TIf
  | TElse
  | TTrue
  | TFalse
  | TColon
  | TAssign
  | TEq
  | TNeq
  | TBang
  | TToI64
  | TToI32
  | TSemicolon
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  deriving (Show, Eq)

data Expr
  = Lit Int
  | BoolLit Bool
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Not Expr
  | ToI64 Expr
  | ToI32 Expr
  deriving (Show, Eq)

data Stmt
  = SLet String Type Expr
  | SAssign String Expr
  | SBlock [Stmt]
  | -- if/else-if*/else?（値を返さない文。分岐は [(条件式, 本体)] の列＋任意のelse本体）
    SIf [(Expr, [Stmt])] (Maybe [Stmt])
  deriving (Show, Eq)

-- 文の列 + 必須の末尾式
type Program = ([Stmt], Expr)

-- 整数演算・変数のビット幅（codegenのレジスタ幅）
data Width = W32 | W64 deriving (Show, Eq)

-- ソースレベルの論理型。Width は整数のレジスタ幅を表すのに対し、
-- Type はレジスタ幅を持たない bool も含めたソース上の型を表す。
data Type = TyInt Width | TBool deriving (Show, Eq)

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
  deriving (Show, Eq)

-- Lexer

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize ('=' : '=' : cs) = (TEq :) <$> tokenize cs
tokenize ('!' : '=' : cs) = (TNeq :) <$> tokenize cs
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize rest
  | isAlpha c =
      let (ident, rest) = span (\ch -> isAlphaNum ch || ch == '_') (c : cs)
       in case ident of
            "let" -> (TLet :) <$> tokenize rest
            "if" -> (TIf :) <$> tokenize rest
            "else" -> (TElse :) <$> tokenize rest
            "true" -> (TTrue :) <$> tokenize rest
            "false" -> (TFalse :) <$> tokenize rest
            "to_i64" -> (TToI64 :) <$> tokenize rest
            "to_i32" -> (TToI32 :) <$> tokenize rest
            _ -> (TIdent ident :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | c == '-' = (TMinus :) <$> tokenize cs
  | c == '*' = (TStar :) <$> tokenize cs
  | c == '/' = (TSlash :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | c == '{' = (TLBrace :) <$> tokenize cs
  | c == '}' = (TRBrace :) <$> tokenize cs
  | c == ':' = (TColon :) <$> tokenize cs
  | c == '=' = (TAssign :) <$> tokenize cs
  | c == '!' = (TBang :) <$> tokenize cs
  | c == ';' = (TSemicolon :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- Parser
--
-- program    ::= stmt* expr
-- stmt       ::= let-stmt | assign-stmt | block-stmt | if-stmt
-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64' | 'bool') '=' expr ';'
-- assign-stmt ::= IDENT '=' expr ';'
-- block-stmt ::= '{' stmt* '}'
-- if-stmt    ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?
-- expr     ::= equality
-- equality ::= additive (('==' | '!=') additive)*
-- additive ::= term (('+' | '-') term)*
-- term     ::= factor (('*' | '/') factor)*
-- factor   ::= INT | 'true' | 'false' | IDENT | '(' expr ')' | '-' factor | '!' factor
--            | 'to_i64' factor | 'to_i32' factor

type ParseResult a = Either String (a, [Token])

-- program ::= stmt* expr
parse :: [Token] -> Either String Program
parse tokens = do
  (stmts, rest) <- parseStmts tokens
  (expr, rest') <- parseEquality rest
  case rest' of
    [] -> Right (stmts, expr)
    (t : _) -> Left ("unexpected token: " ++ show t)

-- 文の抽出
parseStmts :: [Token] -> ParseResult [Stmt]
parseStmts (TLet : rest) = do
  (stmt, rest') <- parseLetStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TIdent name : TAssign : rest) = do
  (stmt, rest') <- parseAssignStmt name rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TLBrace : rest) = do
  (stmt, rest') <- parseBlockStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TIf : rest) = do
  (stmt, rest') <- parseIfStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts tokens = Right ([], tokens)

-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64' | 'bool') '=' expr ';'
parseLetStmt :: [Token] -> ParseResult Stmt
parseLetStmt tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TColon rest
  (tyName, rest'') <- expectIdent rest'
  ty <- parseType tyName
  rest3 <- expectToken TAssign rest''
  (expr, rest4) <- parseEquality rest3
  rest5 <- expectToken TSemicolon rest4
  Right (SLet name ty expr, rest5)

-- 型名 -> Type の変換（現状 i32 / i64 / bool のみ対応）
parseType :: String -> Either String Type
parseType "i32" = Right (TyInt W32)
parseType "i64" = Right (TyInt W64)
parseType "bool" = Right TBool
parseType ty = Left ("unsupported type: " ++ ty)

-- assign-stmt ::= IDENT '=' expr ';'
parseAssignStmt :: String -> [Token] -> ParseResult Stmt
parseAssignStmt name tokens = do
  (expr, rest) <- parseEquality tokens
  rest' <- expectToken TSemicolon rest
  Right (SAssign name expr, rest')

-- block-stmt ::= '{' stmt* '}'（'{' は呼び出し側で消費済み）
parseBlockStmt :: [Token] -> ParseResult Stmt
parseBlockStmt tokens = do
  (stmts, rest) <- parseStmts tokens
  case rest of
    (TRBrace : rest') -> Right (SBlock stmts, rest')
    _ -> Left "expected closing brace"

-- if-stmt ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?（先頭の'if'は呼び出し側で消費済み）
parseIfStmt :: [Token] -> ParseResult Stmt
parseIfStmt tokens = do
  (branch, rest) <- parseIfBranch tokens
  parseIfRest [branch] rest

-- 条件式 + '{' stmt* '}' の1分岐分（'if'/'else if' 共通）
parseIfBranch :: [Token] -> ParseResult (Expr, [Stmt])
parseIfBranch tokens = do
  (cond, rest) <- parseEquality tokens
  rest' <- expectToken TLBrace rest
  (body, rest'') <- parseStmts rest'
  case rest'' of
    (TRBrace : rest3) -> Right ((cond, body), rest3)
    _ -> Left "expected closing brace"

-- 'else if' の継続、末尾の 'else'、あるいはどちらも無ければ確定した分岐列でSIfを組み立てる
parseIfRest :: [(Expr, [Stmt])] -> [Token] -> ParseResult Stmt
parseIfRest acc (TElse : TIf : rest) = do
  (branch, rest') <- parseIfBranch rest
  parseIfRest (acc ++ [branch]) rest'
parseIfRest acc (TElse : TLBrace : rest) = do
  (elseBody, rest') <- parseStmts rest
  case rest' of
    (TRBrace : rest'') -> Right (SIf acc (Just elseBody), rest'')
    _ -> Left "expected closing brace"
parseIfRest _ (TElse : t : _) = Left ("expected 'if' or '{' after 'else', got: " ++ show t)
parseIfRest _ [TElse] = Left "expected 'if' or '{' after 'else', got end of input"
parseIfRest acc rest = Right (SIf acc Nothing, rest)

-- 識別子の抽出（変数、型名など）
expectIdent :: [Token] -> ParseResult String
expectIdent (TIdent name : rest) = Right (name, rest)
expectIdent (t : _) = Left ("expected identifier, got: " ++ show t)
expectIdent [] = Left "expected identifier,  got end of input"

-- 指定トークンの抽出（:, ;など）
expectToken :: Token -> [Token] -> Either String [Token]
expectToken tok (t : rest)
  | t == tok = Right rest
  | otherwise = Left ("expected " ++ show tok ++ ", got: " ++ show t)
expectToken tok [] = Left ("expected " ++ show tok ++ ", got end of input")

-- 等価・非等価の抽出（最低優先順位）
parseEquality :: [Token] -> ParseResult Expr
parseEquality tokens = do
  (left, rest) <- parseExpr tokens
  parseEqualityRest left rest

-- equality ::= additive (('==' | '!=') additive)*
parseEqualityRest :: Expr -> [Token] -> ParseResult Expr
parseEqualityRest left (TEq : rest) = do
  (right, rest') <- parseExpr rest
  parseEqualityRest (Eq left right) rest'
parseEqualityRest left (TNeq : rest) = do
  (right, rest') <- parseExpr rest
  parseEqualityRest (Neq left right) rest'
parseEqualityRest left rest = Right (left, rest)

-- 加算・減算の抽出
parseExpr :: [Token] -> ParseResult Expr
parseExpr tokens = do
  (left, rest) <- parseTerm tokens
  parseExprRest left rest

-- additive ::= term (('+' | '-') term)*
parseExprRest :: Expr -> [Token] -> ParseResult Expr
parseExprRest left (TPlus : rest) = do
  (right, rest') <- parseTerm rest
  parseExprRest (Add left right) rest'
parseExprRest left (TMinus : rest) = do
  (right, rest') <- parseTerm rest
  parseExprRest (Sub left right) rest'
parseExprRest left rest = Right (left, rest)

-- 乗算・除算の抽出
parseTerm :: [Token] -> ParseResult Expr
parseTerm tokens = do
  (left, rest) <- parseFactor tokens
  parseTermRest left rest

-- term ::= factor (('*' | '/') factor)*
parseTermRest :: Expr -> [Token] -> ParseResult Expr
parseTermRest left (TStar : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Mul left right) rest'
parseTermRest left (TSlash : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Div left right) rest'
parseTermRest left rest = Right (left, rest)

-- factor ::= INT | 'true' | 'false' | IDENT | '(' expr ')' | '-' factor | '!' factor
--          | 'to_i64' factor | 'to_i32' factor
parseFactor :: [Token] -> ParseResult Expr
parseFactor (TInt n : rest) = Right (Lit n, rest)
parseFactor (TTrue : rest) = Right (BoolLit True, rest)
parseFactor (TFalse : rest) = Right (BoolLit False, rest)
parseFactor (TIdent name : rest) = Right (Var name, rest)
parseFactor (TLParen : rest) = do
  (expr, rest') <- parseEquality rest
  case rest' of
    (TRParen : rest'') -> Right (expr, rest'')
    _ -> Left "expected closing parenthesis"
parseFactor (TMinus : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Neg expr, rest')
parseFactor (TBang : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Not expr, rest')
parseFactor (TToI64 : rest) = do
  (expr, rest') <- parseFactor rest
  Right (ToI64 expr, rest')
parseFactor (TToI32 : rest) = do
  (expr, rest') <- parseFactor rest
  Right (ToI32 expr, rest')
parseFactor [] = Left "unexpected end of input"
parseFactor (t : _) = Left ("unexpected token: " ++ show t)

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

-- 型の物理格納幅（bool は i32 用の32bitスロットを流用する）
storageWidth :: Type -> Width
storageWidth (TyInt w) = w
storageWidth TBool = W32

-- エラーメッセージ表示用の型名
typeName :: Type -> String
typeName (TyInt W32) = "i32"
typeName (TyInt W64) = "i64"
typeName TBool = "bool"

-- [文]＋式から命令を抽出
compile :: Program -> Either String (Type, [Instr])
compile (stmts, expr) = do
  (env, stmtInstrs) <- compileStmts stmts
  finalType <- inferType env expr
  exprInstrs <- compileExprTyped env finalType expr
  Right (finalType, stmtInstrs ++ exprInstrs)

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

-- 任意のEnv/cursorを起点に[文]から命令を抽出する（ブロック/if分岐の再帰コンパイルに使う）
compileStmtsFrom :: Env -> Int -> [Stmt] -> CompileM (Env, Int, [Instr])
compileStmtsFrom initEnv initCursor stmts = foldM step (initEnv, initCursor, []) stmts
 where
  -- let xxx: 型 = ...
  step (env, cursor, acc) (SLet name ty expr) = do
    -- 変数の二重定義をチェック（同一ブロック内の再宣言のみ対象。外側との同名はシャドーイングとして許可）
    when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
    -- 式の表現から命令を抽出（宣言された型を期待型として渡す）
    instrs <- lift (compileExprTyped env ty expr)
    -- 新しく登録する変数のスタック上のアドレスを、型の物理格納幅分だけ詰めて計算
    let off = cursor - widthBytes (storageWidth ty)
    -- 変数とアドレスのマップ, 命令＋追加命令＋変数のストア
    pure (insertVar name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
  -- xxx = ...
  step (env, cursor, acc) (SAssign name expr) = do
    -- 変数の定義をチェック（外側スコープの変数への書き込みも許可）
    (off, ty) <- lift (maybe (Left ("undeclared variable: " ++ name)) Right (lookupVar name env))
    -- 式の表現から命令を抽出（既存の変数の型を期待型として渡す）
    instrs <- lift (compileExprTyped env ty expr)
    -- 変数とアドレスのマップはそのまま, 命令＋追加命令＋変数のストア
    pure (env, cursor, acc ++ instrs ++ [Store (storageWidth ty) off])
  -- { ... }
  step (env, cursor, acc) (SBlock innerStmts) = do
    -- 先頭に空スコープをpushして再帰コンパイルし、返り値のEnv/cursorは破棄して呼び出し前の値をそのまま継続に使う
    -- （＝スコープアウトとスタックオフセットの巻き戻しを同時に実現する）
    (_, _, instrs) <- compileStmtsFrom (Map.empty : env) cursor innerStmts
    pure (env, cursor, acc ++ instrs)
  -- if 式 {...} (else if 式 {...})* (else {...})?
  step (env, cursor, acc) (SIf branches maybeElse) = do
    instrs <- compileIf env cursor branches maybeElse
    pure (env, cursor, acc ++ instrs)

-- if/else-if/else の分岐列を、条件が偽なら次の分岐へジャンプする形の命令列へ展開する。
-- 各分岐の本体はブロックと同じく独立スコープでコンパイルし、Env/cursorは呼び出し側へ伝播させない
-- （if全体も値を返さない文であり、外側から見た変数の状態はifに入る前と変わらない）。
compileIf :: Env -> Int -> [(Expr, [Stmt])] -> Maybe [Stmt] -> CompileM [Instr]
compileIf env cursor branches maybeElse = do
  endLabel <- freshLabel "if_end"
  body <- go endLabel branches
  pure (body ++ [Label endLabel])
 where
  go _ [] = case maybeElse of
    Nothing -> pure []
    Just elseStmts -> do
      (_, _, instrs) <- compileStmtsFrom (Map.empty : env) cursor elseStmts
      pure instrs
  go endLabel ((cond, body) : rest) = do
    -- 条件式は式木としては expected とは独立にbool型を要求する（if自体はexprを持たない）
    condInstrs <- lift (compileExprTyped env TBool cond)
    nextLabel <- freshLabel "if_next"
    (_, _, bodyInstrs) <- compileStmtsFrom (Map.empty : env) cursor body
    restInstrs <- go endLabel rest
    pure
      ( condInstrs
          ++ [JmpIfZero nextLabel]
          ++ bodyInstrs
          ++ [Jmp endLabel, Label nextLabel]
          ++ restInstrs
      )

-- [文]から命令を抽出
compileStmts :: [Stmt] -> Either String (Env, [Instr])
compileStmts stmts = do
  (env, _cursor, instrs) <- evalStateT (compileStmtsFrom [Map.empty] 0 stmts) 0
  Right (env, instrs)

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

-- 式中の変数参照から型を推論する（整数リテラルのみの部分木は Nothing = 未確定のまま単一化する）。
-- bool リテラルは曖昧さがないため常に Just TBool。
inferMaybeType :: Env -> Expr -> Either String (Maybe Type)
inferMaybeType env = go
 where
  go (Lit _) = Right Nothing
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
  -- to_i64/to_i32 の結果型は被演算子によらず常に確定する（BoolLit と同様、部分木を辿る必要はない）
  go (ToI64 _) = Right (Just (TyInt W64))
  go (ToI32 _) = Right (Just (TyInt W32))
  combine a b = do
    ta <- go a
    tb <- go b
    unifyMaybeType ta tb

-- 最後まで未確定なら i64 をデフォルトとする。
inferType :: Env -> Expr -> Either String Type
inferType env expr = maybe (TyInt W64) id <$> inferMaybeType env expr

-- Eq/Neq の被演算子同士の共通の型を決定する（外側の expected とは独立に推論する）
operandType :: Env -> Expr -> Expr -> Either String Type
operandType env a b = do
  ta <- inferMaybeType env a
  tb <- inferMaybeType env b
  maybe (TyInt W64) id <$> unifyMaybeType ta tb

-- 期待する型（expected）を一様に伝播させながら命令を抽出する。
-- Var の実際の型が expected と食い違えば型不一致エラーとする。
-- 算術演算・整数リテラルは TyInt 専用、Not/BoolLit/Eq/Neq は TBool 専用であり、
-- expected と食い違えばその場でエラーとする。
compileExprTyped :: Env -> Type -> Expr -> Either String [Instr]
-- [let ]xxx = 1234;（リテラルは無型なので expected をそのまま採用する。ただし bool は不可）
compileExprTyped _ TBool (Lit _) = Left "type mismatch: expected bool, found integer literal"
compileExprTyped _ (TyInt _) (Lit n) = Right [Push n]
-- [let ]xxx = true; / false;（bool以外のコンテキストでは不可）
compileExprTyped _ TBool (BoolLit b) = Right [Push (if b then 1 else 0)]
compileExprTyped _ expected@(TyInt _) (BoolLit _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
-- [let ]xxx = yyy;
compileExprTyped env expected (Var name) =
  case lookupVar name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (off, ty)
      | ty /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName ty)
      | otherwise -> Right [Load (storageWidth ty) off]
-- [let ]xxx = expr + expr;
compileExprTyped _ TBool (Add _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped env expected@(TyInt w) (Add l r) = do
  li <- compileExprTyped env expected l
  ri <- compileExprTyped env expected r
  Right (li ++ ri ++ [IAdd w])
-- [let ]xxx = expr - expr;
compileExprTyped _ TBool (Sub _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped env expected@(TyInt w) (Sub l r) = do
  li <- compileExprTyped env expected l
  ri <- compileExprTyped env expected r
  Right (li ++ ri ++ [ISub w])
-- [let ]xxx = expr * expr;
compileExprTyped _ TBool (Mul _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped env expected@(TyInt w) (Mul l r) = do
  li <- compileExprTyped env expected l
  ri <- compileExprTyped env expected r
  Right (li ++ ri ++ [IMul w])
-- [let ]xxx = expr / expr;
compileExprTyped _ TBool (Div _ _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped env expected@(TyInt w) (Div l r) = do
  li <- compileExprTyped env expected l
  ri <- compileExprTyped env expected r
  Right (li ++ ri ++ [IDiv w])
-- [let ]xxx = -expr;
compileExprTyped _ TBool (Neg _) = Left "type mismatch: expected bool, found arithmetic expression"
compileExprTyped env expected@(TyInt w) (Neg e) = do
  ei <- compileExprTyped env expected e
  Right (ei ++ [INeg w])
-- [let ]xxx = !expr;
compileExprTyped _ expected@(TyInt _) (Not _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped env TBool (Not e) = do
  ei <- compileExprTyped env TBool e
  Right (ei ++ [INot])
-- [let ]xxx = expr == expr;（被演算子の型は expected とは独立に推論する）
compileExprTyped _ expected@(TyInt _) (Eq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped env TBool (Eq l r) = do
  opTy <- operandType env l r
  li <- compileExprTyped env opTy l
  ri <- compileExprTyped env opTy r
  Right (li ++ ri ++ [ICmpEq])
-- [let ]xxx = expr != expr;
compileExprTyped _ expected@(TyInt _) (Neq _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found bool")
compileExprTyped env TBool (Neq l r) = do
  opTy <- operandType env l r
  li <- compileExprTyped env opTy l
  ri <- compileExprTyped env opTy r
  Right (li ++ ri ++ [ICmpNe])
-- [let ]xxx = to_i64(expr);（結果は常にi64。被演算子はexpectedとは独立に常にi32を要求する）
compileExprTyped _ TBool (ToI64 _) = Left "type mismatch: expected bool, found i64"
compileExprTyped _ expected@(TyInt W32) (ToI64 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i64")
compileExprTyped env (TyInt W64) (ToI64 e) = do
  ei <- compileExprTyped env (TyInt W32) e
  Right (ei ++ [ISext32])
-- [let ]xxx = to_i32(expr);（結果は常にi32。被演算子はexpectedとは独立に常にi64を要求する）
compileExprTyped _ TBool (ToI32 _) = Left "type mismatch: expected bool, found i32"
compileExprTyped _ expected@(TyInt W64) (ToI32 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i32")
compileExprTyped env (TyInt W32) (ToI32 e) = do
  ei <- compileExprTyped env (TyInt W64) e
  Right (ei ++ [ISext32])

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
  go (INot : rest) (a : stack) vars = go rest ((if a == 0 then 1 else 0) : stack) vars
  go (ISext32 : rest) (a : stack) vars = go rest (trunc W32 a : stack) vars
  go _ _ _ = Left "stack underflow"

-- 実アセンブリの movl/movslq 等が行う切り詰め・符号拡張を再現する
trunc :: Width -> Int -> Int
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)
