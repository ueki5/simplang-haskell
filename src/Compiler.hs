module Compiler (Token (..), Expr (..), Stmt (..), Instr (..), Width (..), Program, tokenize, parse, compile, run) where

import Control.Monad (foldM, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map

data Token
  = TInt Int
  | TIdent String
  | TLet
  | TColon
  | TAssign
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
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show, Eq)

data Stmt
  = SLet String Width Expr
  | SAssign String Expr
  deriving (Show, Eq)

-- 文の列 + 必須の末尾式
type Program = ([Stmt], Expr)

-- 変数・演算のビット幅（ソースの型名から codegen のレジスタ幅までを1つの型で兼用する）
data Width = W32 | W64 deriving (Show, Eq)

data Instr
  = Push Int
  | IAdd Width
  | ISub Width
  | IMul Width
  | IDiv Width
  | INeg Width
  | Load Width Int
  | Store Width Int
  deriving (Show, Eq)

-- Lexer

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TInt (read digits) :) <$> tokenize rest
  | isAlpha c =
      let (ident, rest) = span (\ch -> isAlphaNum ch || ch == '_') (c : cs)
       in if ident == "let"
            then (TLet :) <$> tokenize rest
            else (TIdent ident :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | c == '-' = (TMinus :) <$> tokenize cs
  | c == '*' = (TStar :) <$> tokenize cs
  | c == '/' = (TSlash :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | c == ':' = (TColon :) <$> tokenize cs
  | c == '=' = (TAssign :) <$> tokenize cs
  | c == ';' = (TSemicolon :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- Parser
--
-- program ::= stmt* expr
-- stmt    ::= let-stmt | assign-stmt
-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64') '=' expr ';'
-- assign-stmt ::= IDENT '=' expr ';'
-- expr   ::= term   (('+' | '-') term)*
-- term   ::= factor (('*' | '/') factor)*
-- factor ::= INT | IDENT | '(' expr ')' | '-' factor

type ParseResult a = Either String (a, [Token])

-- program ::= stmt* expr
parse :: [Token] -> Either String Program
parse tokens = do
  (stmts, rest) <- parseStmts tokens
  (expr, rest') <- parseExpr rest
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

-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64') '=' expr ';'
parseLetStmt :: [Token] -> ParseResult Stmt
parseLetStmt tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TColon rest
  (tyName, rest'') <- expectIdent rest'
  width <- parseType tyName
  rest3 <- expectToken TAssign rest''
  (expr, rest4) <- parseExpr rest3
  rest5 <- expectToken TSemicolon rest4
  Right (SLet name width expr, rest5)

-- 型名 -> Width の変換（現状 i32 / i64 のみ対応）
parseType :: String -> Either String Width
parseType "i32" = Right W32
parseType "i64" = Right W64
parseType ty = Left ("unsupported type: " ++ ty)

-- assign-stmt ::= IDENT '=' expr ';'
parseAssignStmt :: String -> [Token] -> ParseResult Stmt
parseAssignStmt name tokens = do
  (expr, rest) <- parseExpr tokens
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

-- 加算・減算の抽出
parseExpr :: [Token] -> ParseResult Expr
parseExpr tokens = do
  (left, rest) <- parseTerm tokens
  parseExprRest left rest

-- expr   ::= term   (('+' | '-') term)*
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

-- term   ::= factor (('*' | '/') factor)*
parseTermRest :: Expr -> [Token] -> ParseResult Expr
parseTermRest left (TStar : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Mul left right) rest'
parseTermRest left (TSlash : rest) = do
  (right, rest') <- parseFactor rest
  parseTermRest (Div left right) rest'
parseTermRest left rest = Right (left, rest)

-- factor ::= INT | IDENT | '(' expr ')' | '-' factor
parseFactor :: [Token] -> ParseResult Expr
parseFactor (TInt n : rest) = Right (Lit n, rest)
parseFactor (TIdent name : rest) = Right (Var name, rest)
parseFactor (TLParen : rest) = do
  (expr, rest') <- parseExpr rest
  case rest' of
    (TRParen : rest'') -> Right (expr, rest'')
    _ -> Left "expected closing parenthesis"
parseFactor (TMinus : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Neg expr, rest')
parseFactor [] = Left "unexpected end of input"
parseFactor (t : _) = Left ("unexpected token: " ++ show t)

-- Code generator

-- 変数名 -> (%rbp相対オフセット, 型)
type Env = Map String (Int, Width)

-- 型ごとのスタック占有バイト数
widthBytes :: Width -> Int
widthBytes W32 = 4
widthBytes W64 = 8

-- エラーメッセージ表示用の型名
typeName :: Width -> String
typeName W32 = "i32"
typeName W64 = "i64"

-- [文]＋式から命令を抽出
compile :: Program -> Either String (Width, [Instr])
compile (stmts, expr) = do
  (env, stmtInstrs) <- compileStmts stmts
  finalWidth <- inferType env expr
  exprInstrs <- compileExprTyped env finalWidth expr
  Right (finalWidth, stmtInstrs ++ exprInstrs)

-- [文]から命令を抽出
compileStmts :: [Stmt] -> Either String (Env, [Instr])
compileStmts stmts = do
  (env, _cursor, instrs) <- foldM step (Map.empty, 0, []) stmts
  Right (env, instrs)
 where
  -- let xxx: 型 = ...
  step (env, cursor, acc) (SLet name width expr) = do
    -- 変数の二重定義をチェック
    when (Map.member name env) $ Left ("variable already declared: " ++ name)
    -- 式の表現から命令を抽出（宣言された型を期待幅として渡す）
    instrs <- compileExprTyped env width expr
    -- 新しく登録する変数のスタック上のアドレスを、型の実サイズ分だけ詰めて計算
    let off = cursor - widthBytes width
    -- 変数とアドレスのマップ, 命令＋追加命令＋変数のストア
    Right (Map.insert name (off, width) env, off, acc ++ instrs ++ [Store width off])
  -- xxx = ...
  step (env, cursor, acc) (SAssign name expr) = do
    -- 変数の定義をチェック
    (off, width) <- maybe (Left ("undeclared variable: " ++ name)) Right (Map.lookup name env)
    -- 式の表現から命令を抽出（既存の変数の型を期待幅として渡す）
    instrs <- compileExprTyped env width expr
    -- 変数とアドレスのマップはそのまま, 命令＋追加命令＋変数のストア
    Right (env, cursor, acc ++ instrs ++ [Store width off])

-- 式中の変数参照から型を推論する（リテラルのみの部分木は Nothing = 未確定のまま単一化する）。
-- 最後まで未確定なら i64 をデフォルトとする。
inferType :: Env -> Expr -> Either String Width
inferType env expr = maybe W64 id <$> go expr
 where
  go (Lit _) = Right Nothing
  go (Var name) =
    maybe (Left ("undeclared variable: " ++ name)) (Right . Just . snd) (Map.lookup name env)
  go (Neg e) = go e
  go (Add a b) = combine a b
  go (Sub a b) = combine a b
  go (Mul a b) = combine a b
  go (Div a b) = combine a b
  combine a b = do
    wa <- go a
    wb <- go b
    unify wa wb
  unify Nothing Nothing = Right Nothing
  unify Nothing (Just w) = Right (Just w)
  unify (Just w) Nothing = Right (Just w)
  unify (Just w1) (Just w2)
    | w1 == w2 = Right (Just w1)
    | otherwise = Left ("type mismatch: " ++ typeName w1 ++ " and " ++ typeName w2)

-- 期待する型（expected）を一様に伝播させながら命令を抽出する。
-- Var の実際の型が expected と食い違えば型混在エラーとする。
compileExprTyped :: Env -> Width -> Expr -> Either String [Instr]
-- [let ]xxx = 1234;（リテラルは無型なので expected をそのまま採用する）
compileExprTyped _ _ (Lit n) = Right [Push n]
-- [let ]xxx = yyy;
compileExprTyped env expected (Var name) =
  case Map.lookup name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (off, width)
      | width /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName width)
      | otherwise -> Right [Load width off]
-- [let ]xxx = expr + expr;
compileExprTyped env expected (Add l r) = binOp IAdd l r
 where
  binOp op a b = do
    ai <- compileExprTyped env expected a
    bi <- compileExprTyped env expected b
    Right (ai ++ bi ++ [op expected])
-- [let ]xxx = expr - expr;
compileExprTyped env expected (Sub l r) = binOp ISub l r
 where
  binOp op a b = do
    ai <- compileExprTyped env expected a
    bi <- compileExprTyped env expected b
    Right (ai ++ bi ++ [op expected])
-- [let ]xxx = expr * expr;
compileExprTyped env expected (Mul l r) = binOp IMul l r
 where
  binOp op a b = do
    ai <- compileExprTyped env expected a
    bi <- compileExprTyped env expected b
    Right (ai ++ bi ++ [op expected])
-- [let ]xxx = expr / expr;
compileExprTyped env expected (Div l r) = binOp IDiv l r
 where
  binOp op a b = do
    ai <- compileExprTyped env expected a
    bi <- compileExprTyped env expected b
    Right (ai ++ bi ++ [op expected])
-- [let ]xxx = -expr;
compileExprTyped env expected (Neg e) = do
  ei <- compileExprTyped env expected e
  Right (ei ++ [INeg expected])

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
  go _ _ _ = Left "stack underflow"

-- 実アセンブリの movl/movslq 等が行う切り詰め・符号拡張を再現する
trunc :: Width -> Int -> Int
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)
