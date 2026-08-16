module Parser (Token (..), Expr (..), Stmt (..), FnDecl (..), Width (..), Type (..), Program, tokenize, parse) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Token
  = TInt Int
  | TIdent String
  | TLet
  | TIf
  | TElse
  | TWhile
  | TBreak
  | TContinue
  | TTrue
  | TFalse
  | TFn
  | TReturn
  | TArrow
  | TComma
  | TColon
  | TAssign
  | TEq
  | TNeq
  | TLt
  | TLe
  | TGt
  | TGe
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
  | Lt Expr Expr
  | Le Expr Expr
  | Gt Expr Expr
  | Ge Expr Expr
  | Not Expr
  | ToI64 Expr
  | ToI32 Expr
  | Call String [Expr]
  deriving (Show, Eq)

data Stmt
  = SLet String Type Expr
  | SAssign String Expr
  | SBlock [Stmt]
  | -- if/else-if*/else?（値を返さない文。分岐は [(条件式, 本体)] の列＋任意のelse本体）
    SIf [(Expr, [Stmt])] (Maybe [Stmt])
  | -- while 条件式 {...}（条件が真である間、本体を繰り返す）
    SWhile Expr [Stmt]
  | -- 直近の外側ループを抜ける
    SBreak
  | -- 直近の外側ループの条件再評価へ進む
    SContinue
  | -- 関数の早期リターン（値は必須。ユニット型は採用しないため値なし版は無い）
    SReturn Expr
  deriving (Show, Eq)

-- 文の列 + 必須の末尾式（関数本体・プログラム全体で共通の形）
type Body = ([Stmt], Expr)

-- 関数定義: 名前, 仮引数(名前, 型)の列, 戻り値の型, 本体
data FnDecl = FnDecl String [(String, Type)] Type Body
  deriving (Show, Eq)

-- fn定義の列 + 既存の暗黙main本体
type Program = ([FnDecl], [Stmt], Expr)

-- 整数演算・変数のビット幅（codegenのレジスタ幅）
data Width = W32 | W64 deriving (Show, Eq)

-- ソースレベルの論理型。Width は整数のレジスタ幅を表すのに対し、
-- Type はレジスタ幅を持たない bool も含めたソース上の型を表す。
data Type = TyInt Width | TBool deriving (Show, Eq)

-- Lexer

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize ('=' : '=' : cs) = (TEq :) <$> tokenize cs
tokenize ('!' : '=' : cs) = (TNeq :) <$> tokenize cs
tokenize ('<' : '=' : cs) = (TLe :) <$> tokenize cs
tokenize ('>' : '=' : cs) = (TGe :) <$> tokenize cs
tokenize ('-' : '>' : cs) = (TArrow :) <$> tokenize cs
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
            "while" -> (TWhile :) <$> tokenize rest
            "break" -> (TBreak :) <$> tokenize rest
            "continue" -> (TContinue :) <$> tokenize rest
            "true" -> (TTrue :) <$> tokenize rest
            "false" -> (TFalse :) <$> tokenize rest
            "fn" -> (TFn :) <$> tokenize rest
            "return" -> (TReturn :) <$> tokenize rest
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
  | c == ',' = (TComma :) <$> tokenize cs
  | c == ':' = (TColon :) <$> tokenize cs
  | c == '=' = (TAssign :) <$> tokenize cs
  | c == '!' = (TBang :) <$> tokenize cs
  | c == '<' = (TLt :) <$> tokenize cs
  | c == '>' = (TGt :) <$> tokenize cs
  | c == ';' = (TSemicolon :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- Parser
--
-- program    ::= (fn-decl | stmt)* expr
-- fn-decl    ::= 'fn' IDENT '(' (IDENT ':' 型 (',' IDENT ':' 型)*)? ')' '->' 型 '{' stmt* expr '}'
-- stmt       ::= let-stmt | assign-stmt | block-stmt | if-stmt | while-stmt | break-stmt | continue-stmt | return-stmt
-- let-stmt    ::= 'let' IDENT ':' ('i32' | 'i64' | 'bool') '=' expr ';'
-- assign-stmt ::= IDENT '=' expr ';'
-- block-stmt ::= '{' stmt* '}'
-- if-stmt    ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?
-- while-stmt    ::= 'while' expr '{' stmt* '}'
-- break-stmt    ::= 'break' ';'
-- continue-stmt ::= 'continue' ';'
-- return-stmt   ::= 'return' expr ';'
-- expr       ::= equality
-- equality   ::= comparison (('==' | '!=') comparison)*
-- comparison ::= additive (('<' | '<=' | '>' | '>=') additive)*
-- additive   ::= multiplicative (('+' | '-') multiplicative)*
-- multiplicative       ::= factor (('*' | '/') factor)*
-- factor     ::= INT | 'true' | 'false' | IDENT | IDENT '(' (expr (',' expr)*)? ')'
--              | '(' expr ')' | '-' factor | '!' factor | 'to_i64' factor | 'to_i32' factor

type ParseResult a = Either String (a, [Token])

-- program ::= (fn-decl | stmt)* expr
parse :: [Token] -> Either String Program
parse tokens = do
  (fnDecls, stmts, rest) <- parseTopLevel tokens
  (expr, rest') <- parseExpr rest
  case rest' of
    [] -> Right (fnDecls, stmts, expr)
    (t : _) -> Left ("unexpected token: " ++ show t)

-- トップレベルの (fn定義 | 文)* を読む。fn定義は既存のstmtの列とは独立に集約する
-- （ネストしたブロック/if/while本体はparseStmtsをそのまま使うため、その内部にfn定義は出現できない）。
-- 文の連続はparseStmtsで一気に読み、止まった位置が'fn'であれば再帰してさらに読み進める
parseTopLevel :: [Token] -> Either String ([FnDecl], [Stmt], [Token])
parseTopLevel (TFn : rest) = do
  (fnDecl, rest') <- parseFnDecl rest
  (fnDecls, stmts, rest'') <- parseTopLevel rest'
  Right (fnDecl : fnDecls, stmts, rest'')
parseTopLevel tokens = do
  (stmts, rest) <- parseStmts tokens
  case rest of
    (TFn : _) -> do
      (fnDecls, stmts', rest') <- parseTopLevel rest
      Right (fnDecls, stmts ++ stmts', rest')
    _ -> Right ([], stmts, rest)

-- fn定義（先頭の'fn'は呼び出し側で消費済み）
parseFnDecl :: [Token] -> ParseResult FnDecl
parseFnDecl tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TLParen rest
  (params, rest'') <- parseParamList rest'
  rest3 <- expectToken TArrow rest''
  (tyName, rest4) <- expectIdent rest3
  retTy <- parseType tyName
  rest5 <- expectToken TLBrace rest4
  (stmts, rest6) <- parseStmts rest5
  (tailExpr, rest7) <- parseExpr rest6
  case rest7 of
    (TRBrace : rest8) -> Right (FnDecl name params retTy (stmts, tailExpr), rest8)
    _ -> Left "expected closing brace"

-- 仮引数リスト: (IDENT ':' 型 (',' IDENT ':' 型)*)?（'(' は呼び出し側で消費済み、終端の ')' はここで消費する）
parseParamList :: [Token] -> ParseResult [(String, Type)]
parseParamList (TRParen : rest) = Right ([], rest)
parseParamList tokens = do
  (param, rest) <- parseParam tokens
  parseParamListRest [param] rest

-- IDENT ':' 型
parseParam :: [Token] -> ParseResult (String, Type)
parseParam tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TColon rest
  (tyName, rest'') <- expectIdent rest'
  ty <- parseType tyName
  Right ((name, ty), rest'')

parseParamListRest :: [(String, Type)] -> [Token] -> ParseResult [(String, Type)]
parseParamListRest acc (TComma : rest) = do
  (param, rest') <- parseParam rest
  parseParamListRest (acc ++ [param]) rest'
parseParamListRest acc (TRParen : rest) = Right (acc, rest)
parseParamListRest _ (t : _) = Left ("expected ',' or ')', got: " ++ show t)
parseParamListRest _ [] = Left "expected ',' or ')', got end of input"

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
parseStmts (TWhile : rest) = do
  (stmt, rest') <- parseWhileStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TBreak : rest) = do
  (stmt, rest') <- parseBreakStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TContinue : rest) = do
  (stmt, rest') <- parseContinueStmt rest
  (stmts, rest'') <- parseStmts rest'
  Right (stmt : stmts, rest'')
parseStmts (TReturn : rest) = do
  (stmt, rest') <- parseReturnStmt rest
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
  (expr, rest4) <- parseExpr rest3
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
  (expr, rest) <- parseExpr tokens
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
  (cond, rest) <- parseExpr tokens
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

-- while-stmt ::= 'while' expr '{' stmt* '}'（先頭の'while'は呼び出し側で消費済み）
parseWhileStmt :: [Token] -> ParseResult Stmt
parseWhileStmt tokens = do
  (cond, rest) <- parseExpr tokens
  rest' <- expectToken TLBrace rest
  (body, rest'') <- parseStmts rest'
  case rest'' of
    (TRBrace : rest3) -> Right (SWhile cond body, rest3)
    _ -> Left "expected closing brace"

-- break-stmt ::= 'break' ';'（先頭の'break'は呼び出し側で消費済み）
parseBreakStmt :: [Token] -> ParseResult Stmt
parseBreakStmt tokens = do
  rest <- expectToken TSemicolon tokens
  Right (SBreak, rest)

-- continue-stmt ::= 'continue' ';'（先頭の'continue'は呼び出し側で消費済み）
parseContinueStmt :: [Token] -> ParseResult Stmt
parseContinueStmt tokens = do
  rest <- expectToken TSemicolon tokens
  Right (SContinue, rest)

-- return-stmt ::= 'return' expr ';'（先頭の'return'は呼び出し側で消費済み。値は必須）
parseReturnStmt :: [Token] -> ParseResult Stmt
parseReturnStmt tokens = do
  (expr, rest) <- parseExpr tokens
  rest' <- expectToken TSemicolon rest
  Right (SReturn expr, rest')

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

{- 参考：haskellにおける演算子の優先度
infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /
infixl 6  +, -
infixr 5  ++, :
infix  4  ==, /=, <, <=, >, >=
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =
infixr 0  $, $!
-}

-- 式の抽出
parseExpr :: [Token] -> ParseResult Expr
parseExpr = parseEquality

-- 等価・非等価の抽出（最低優先順位）
parseEquality :: [Token] -> ParseResult Expr
parseEquality tokens = do
  (left, rest) <- parseComparison tokens
  parseEqualityRest left rest

-- equality ::= comparison (('==' | '!=') comparison)*
parseEqualityRest :: Expr -> [Token] -> ParseResult Expr
parseEqualityRest left (TEq : rest) = do
  (right, rest') <- parseComparison rest
  parseEqualityRest (Eq left right) rest'
parseEqualityRest left (TNeq : rest) = do
  (right, rest') <- parseComparison rest
  parseEqualityRest (Neq left right) rest'
parseEqualityRest left rest = Right (left, rest)

-- 大小比較の抽出（等価より高い優先順位、加減算より低い優先順位）
parseComparison :: [Token] -> ParseResult Expr
parseComparison tokens = do
  (left, rest) <- parseAdditive tokens
  parseComparisonRest left rest

-- comparison ::= additive (('<' | '<=' | '>' | '>=') additive)*
parseComparisonRest :: Expr -> [Token] -> ParseResult Expr
parseComparisonRest left (TLt : rest) = do
  (right, rest') <- parseAdditive rest
  parseComparisonRest (Lt left right) rest'
parseComparisonRest left (TLe : rest) = do
  (right, rest') <- parseAdditive rest
  parseComparisonRest (Le left right) rest'
parseComparisonRest left (TGt : rest) = do
  (right, rest') <- parseAdditive rest
  parseComparisonRest (Gt left right) rest'
parseComparisonRest left (TGe : rest) = do
  (right, rest') <- parseAdditive rest
  parseComparisonRest (Ge left right) rest'
parseComparisonRest left rest = Right (left, rest)

-- 加算・減算の抽出
parseAdditive :: [Token] -> ParseResult Expr
parseAdditive tokens = do
  (left, rest) <- parseMultiplicative tokens
  parseAdditiveRest left rest

-- additive ::= multiplicative (('+' | '-') multiplicative)*
parseAdditiveRest :: Expr -> [Token] -> ParseResult Expr
parseAdditiveRest left (TPlus : rest) = do
  (right, rest') <- parseMultiplicative rest
  parseAdditiveRest (Add left right) rest'
parseAdditiveRest left (TMinus : rest) = do
  (right, rest') <- parseMultiplicative rest
  parseAdditiveRest (Sub left right) rest'
parseAdditiveRest left rest = Right (left, rest)

-- 乗算・除算の抽出
parseMultiplicative :: [Token] -> ParseResult Expr
parseMultiplicative tokens = do
  (left, rest) <- parseFactor tokens
  parseMultiplicativeRest left rest

-- multiplicative ::= factor (('*' | '/') factor)*
parseMultiplicativeRest :: Expr -> [Token] -> ParseResult Expr
parseMultiplicativeRest left (TStar : rest) = do
  (right, rest') <- parseFactor rest
  parseMultiplicativeRest (Mul left right) rest'
parseMultiplicativeRest left (TSlash : rest) = do
  (right, rest') <- parseFactor rest
  parseMultiplicativeRest (Div left right) rest'
parseMultiplicativeRest left rest = Right (left, rest)

-- factor ::= INT | 'true' | 'false' | IDENT | IDENT '(' (expr (',' expr)*)? ')'
--          | '(' expr ')' | '-' factor | '!' factor | 'to_i64' factor | 'to_i32' factor
parseFactor :: [Token] -> ParseResult Expr
parseFactor (TInt n : rest) = Right (Lit n, rest)
parseFactor (TTrue : rest) = Right (BoolLit True, rest)
parseFactor (TFalse : rest) = Right (BoolLit False, rest)
parseFactor (TIdent name : TLParen : rest) = do
  (args, rest') <- parseArgList rest
  Right (Call name args, rest')
parseFactor (TIdent name : rest) = Right (Var name, rest)
parseFactor (TLParen : rest) = do
  (expr, rest') <- parseExpr rest
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

-- 実引数リスト: (expr (',' expr)*)?（'(' は呼び出し側で消費済み、終端の ')' はここで消費する）
parseArgList :: [Token] -> ParseResult [Expr]
parseArgList (TRParen : rest) = Right ([], rest)
parseArgList tokens = do
  (arg, rest) <- parseExpr tokens
  parseArgListRest [arg] rest

parseArgListRest :: [Expr] -> [Token] -> ParseResult [Expr]
parseArgListRest acc (TComma : rest) = do
  (arg, rest') <- parseExpr rest
  parseArgListRest (acc ++ [arg]) rest'
parseArgListRest acc (TRParen : rest) = Right (acc, rest)
parseArgListRest _ (t : _) = Left ("expected ',' or ')', got: " ++ show t)
parseArgListRest _ [] = Left "expected ',' or ')', got end of input"
