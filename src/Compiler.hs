module Compiler (Token (..), Expr (..), Stmt (..), Instr (..), Width (..), Type (..), Program, tokenize, parse, compile, run) where

import Control.Monad (foldM, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map

data Token
  = TInt Int
  | TIdent String
  | TLet
  | TTrue
  | TFalse
  | TColon
  | TAssign
  | TEq
  | TNeq
  | TBang
  | TSemicolon
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
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
  deriving (Show, Eq)

data Stmt
  = SLet String Type Expr
  | SAssign String Expr
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
            "true" -> (TTrue :) <$> tokenize rest
            "false" -> (TFalse :) <$> tokenize rest
            _ -> (TIdent ident :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | c == '-' = (TMinus :) <$> tokenize cs
  | c == '*' = (TStar :) <$> tokenize cs
  | c == '/' = (TSlash :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | c == ':' = (TColon :) <$> tokenize cs
  | c == '=' = (TAssign :) <$> tokenize cs
  | c == '!' = (TBang :) <$> tokenize cs
  | c == ';' = (TSemicolon :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- Parser
--
-- program ::= stmt* expr
-- stmt    ::= let-stmt | assign-stmt
-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64' | 'bool') '=' expr ';'
-- assign-stmt ::= IDENT '=' expr ';'
-- expr     ::= equality
-- equality ::= additive (('==' | '!=') additive)*
-- additive ::= term (('+' | '-') term)*
-- term     ::= factor (('*' | '/') factor)*
-- factor   ::= INT | 'true' | 'false' | IDENT | '(' expr ')' | '-' factor | '!' factor

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
parseFactor [] = Left "unexpected end of input"
parseFactor (t : _) = Left ("unexpected token: " ++ show t)

-- Code generator

-- 変数名 -> (%rbp相対オフセット, 型)
type Env = Map String (Int, Type)

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

-- [文]から命令を抽出
compileStmts :: [Stmt] -> Either String (Env, [Instr])
compileStmts stmts = do
  (env, _cursor, instrs) <- foldM step (Map.empty, 0, []) stmts
  Right (env, instrs)
 where
  -- let xxx: 型 = ...
  step (env, cursor, acc) (SLet name ty expr) = do
    -- 変数の二重定義をチェック
    when (Map.member name env) $ Left ("variable already declared: " ++ name)
    -- 式の表現から命令を抽出（宣言された型を期待型として渡す）
    instrs <- compileExprTyped env ty expr
    -- 新しく登録する変数のスタック上のアドレスを、型の物理格納幅分だけ詰めて計算
    let off = cursor - widthBytes (storageWidth ty)
    -- 変数とアドレスのマップ, 命令＋追加命令＋変数のストア
    Right (Map.insert name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
  -- xxx = ...
  step (env, cursor, acc) (SAssign name expr) = do
    -- 変数の定義をチェック
    (off, ty) <- maybe (Left ("undeclared variable: " ++ name)) Right (Map.lookup name env)
    -- 式の表現から命令を抽出（既存の変数の型を期待型として渡す）
    instrs <- compileExprTyped env ty expr
    -- 変数とアドレスのマップはそのまま, 命令＋追加命令＋変数のストア
    Right (env, cursor, acc ++ instrs ++ [Store (storageWidth ty) off])

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
    maybe (Left ("undeclared variable: " ++ name)) (Right . Just . snd) (Map.lookup name env)
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
  case Map.lookup name env of
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
  go _ _ _ = Left "stack underflow"

-- 実アセンブリの movl/movslq 等が行う切り詰め・符号拡張を再現する
trunc :: Width -> Int -> Int
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)
