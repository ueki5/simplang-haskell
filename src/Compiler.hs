module Compiler (Token (..), Expr (..), Stmt (..), Instr (..), Program, tokenize, parse, compile, run) where

import Control.Monad (foldM, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
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
  = SLet String Expr
  | SAssign String Expr
  deriving (Show, Eq)

-- 文の列 + 必須の末尾式
type Program = ([Stmt], Expr)

data Instr
  = Push Int
  | IAdd
  | ISub
  | IMul
  | IDiv
  | INeg
  | Load Int
  | Store Int
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
-- let-stmt    ::= 'let' IDENT ':' 'i64' '=' expr ';'
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

-- let-stmt    ::= 'let' IDENT ':' 'i64' '=' expr ';'
parseLetStmt :: [Token] -> ParseResult Stmt
parseLetStmt tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TColon rest
  (ty, rest'') <- expectIdent rest'
  if ty /= "i64"
    then Left ("unsupported type: " ++ ty)
    else do
      rest3 <- expectToken TAssign rest''
      (expr, rest4) <- parseExpr rest3
      rest5 <- expectToken TSemicolon rest4
      Right (SLet name expr, rest5)

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

-- [文]＋式から命令を抽出
compile :: Program -> Either String [Instr]
compile (stmts, expr) = do
  (env, stmtInstrs) <- compileStmts Map.empty stmts
  exprInstrs <- compileExpr env expr
  Right (stmtInstrs ++ exprInstrs)

-- [文]から命令を抽出
compileStmts :: Map String Int -> [Stmt] -> Either String (Map String Int, [Instr])
compileStmts env0 = foldM step (env0, [])
 where
  -- let xxx = ...
  step (env, acc) (SLet name expr) = do
    -- 変数の二重定義をチェック
    when (Map.member name env) $ Left ("variable already declared: " ++ name)
    -- 式の表現から命令を抽出
    instrs <- compileExpr env expr
    -- 新しく登録する変数のスタック上のアドレスを計算
    let off = -8 * (Map.size env + 1)
    -- 変数とアドレスのマップ, 命令＋追加命令＋変数のストア
    Right (Map.insert name off env, acc ++ instrs ++ [Store off])
  -- xxx = ...
  step (env, acc) (SAssign name expr) = do
    -- 変数の定義をチェック
    off <- maybe (Left ("undeclared variable: " ++ name)) Right (Map.lookup name env)
    -- 式の表現から命令を抽出
    instrs <- compileExpr env expr
    -- マップはそのまま, 命令＋追加命令＋変数のストア
    Right (env, acc ++ instrs ++ [Store off])

-- 式から命令を抽出
compileExpr :: Map String Int -> Expr -> Either String [Instr]
-- [let ]xxx = 1234;
compileExpr _ (Lit n) = Right [Push n]
-- [let ]xxx = yyy;
compileExpr env (Var name) =
  maybe (Left ("undeclared variable: " ++ name)) (\off -> Right [Load off]) (Map.lookup name env)
-- [let ]xxx = expr + expr;
compileExpr env (Add l r) = do
  li <- compileExpr env l
  ri <- compileExpr env r
  Right (li ++ ri ++ [IAdd])
-- [let ]xxx = expr - expr;
compileExpr env (Sub l r) = do
  li <- compileExpr env l
  ri <- compileExpr env r
  Right (li ++ ri ++ [ISub])
-- [let ]xxx = expr * expr;
compileExpr env (Mul l r) = do
  li <- compileExpr env l
  ri <- compileExpr env r
  Right (li ++ ri ++ [IMul])
-- [let ]xxx = expr / expr;
compileExpr env (Div l r) = do
  li <- compileExpr env l
  ri <- compileExpr env r
  Right (li ++ ri ++ [IDiv])
-- [let ]xxx = -expr;
compileExpr env (Neg e) = do
  ei <- compileExpr env e
  Right (ei ++ [INeg])

-- Virtual machine

-- テストでのみ使用
run :: [Instr] -> Either String Int
run instrs = go instrs [] Map.empty
 where
  go [] [v] _ = Right v
  go [] _ _ = Left "invalid stack state after execution"
  go (Push n : rest) stack vars = go rest (n : stack) vars
  go (IAdd : rest) (b : a : stack) vars = go rest ((a + b) : stack) vars
  go (ISub : rest) (b : a : stack) vars = go rest ((a - b) : stack) vars
  go (IMul : rest) (b : a : stack) vars = go rest ((a * b) : stack) vars
  go (IDiv : rest) (b : a : stack) vars
    | b == 0 = Left "division by zero"
    | otherwise = go rest ((a `div` b) : stack) vars
  go (INeg : rest) (a : stack) vars = go rest (negate a : stack) vars
  go (Load off : rest) stack vars =
    case Map.lookup off vars of
      Just v -> go rest (v : stack) vars
      Nothing -> Left "uninitialized variable"
  go (Store off : rest) (v : stack) vars = go rest stack (Map.insert off v vars)
  go _ _ _ = Left "stack underflow"
