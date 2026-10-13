module Parser (Token (..), Expr (..), Stmt (..), FnDecl (..), Width (..), Signedness (..), Type (..), Program, intRepr, tokenize, parse) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (stripPrefix)

data Token
  = TInt Int
  | -- 'i32'/'i64'/'u32'/'u64'/'usize'サフィックス付き整数リテラル（例: `9999i64`）。空白を挟まず直接連結される。
    -- 型は常に整数型（intRepr が Just を返す型）
    TIntSuffixed Int Type
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
  | TAs
  | TSemicolon
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TAmp
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  deriving (Show, Eq)

data Expr
  = Lit Int
  | -- 'i32'/'i64'/'u32'/'u64'/'usize'サフィックスで型を固定した整数リテラル（例: `9999i64`）。Litと異なりexpectedとは無関係に型が確定する
    LitTyped Int Type
  | BoolLit Bool
  | -- ユニット値 `()`。fn本体の末尾式の省略や `return;` もパーサでこれへ脱糖する
    UnitLit
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
  | -- `expr as i64` / `expr as usize` など（キャスト先は整数型のみ）
    Cast Expr Type
  | Call String [Expr]
  | AddrOf Expr
  | Deref Expr
  | -- 配列リテラル `[e1, e2, ...]`（letの初期化式でのみ使える）
    ArrayLit [Expr]
  | -- 繰り返し配列リテラル `[e; N]`（letの初期化式でのみ使える。Nは無型の整数リテラル）
    ArrayRepeat Expr Int
  | -- 添字アクセス `a[i]`（添字はusize型）
    Index Expr Expr
  deriving (Show, Eq)

data Stmt
  = SLet String Type Expr
  | -- 型注釈を省略したlet（例: `let x = 5;`）。型はcompileExprTyped側でinferTypeにより推論する
    SLetInferred String Expr
  | SAssign String Expr
  | -- 場所式（配列要素・デリファレンス）への代入 `a[i] = expr;` / `p[i] = expr;` / `*p = expr;`
    SAssignPlace Expr Expr
  | SBlock [Stmt]
  | -- if/else-if*/else?（値を返さない文。分岐は [(条件式, 本体)] の列＋任意のelse本体）
    SIf [(Expr, [Stmt])] (Maybe [Stmt])
  | -- while 条件式 {...}（条件が真である間、本体を繰り返す）
    SWhile Expr [Stmt]
  | -- 直近の外側ループを抜ける
    SBreak
  | -- 直近の外側ループの条件再評価へ進む
    SContinue
  | -- 関数の早期リターン（値を省略した `return;` は `SReturn UnitLit` へ脱糖する）
    SReturn Expr
  | -- 式文 `expr;`（式を評価して値を捨てる。値の型は問わない）
    SExpr Expr
  deriving (Show, Eq)

-- 文の列 + 末尾式（関数本体の形。省略された末尾式は UnitLit へ脱糖済み）
type Body = ([Stmt], Expr)

-- 関数定義: 名前, 仮引数(名前, 型)の列, 戻り値の型, 本体。
-- シグネチャは宣言だけで確定する（仮引数の型注釈は必須。'->' を省略した戻り値型は () とする）
data FnDecl = FnDecl String [(String, Type)] Type Body
  deriving (Show, Eq)

-- fn定義の列（main を含む。トップレベルにはfn定義しか書けない）
type Program = [FnDecl]

-- 整数演算・変数のビット幅（codegenのレジスタ幅）
data Width = W32 | W64 deriving (Show, Eq)

-- 整数の符号の有無（i32/i64 は Signed、u32/u64 は Unsigned）
data Signedness = Signed | Unsigned deriving (Show, Eq)

-- ソースレベルの論理型。Width は整数のレジスタ幅を表すのに対し、
-- Type はレジスタ幅を持たない bool・再帰的なポインタ型・配列型も含めたソース上の型を表す。
-- TUsize は配列の添字用の型で、表現・命令列は u64 と同一だが u64 とは別の型として扱う（Rust準拠）。
-- TArray の要素型は配列以外の型のみ（ネストした配列は扱わない）。
-- TUnit はユニット型 `()`（値は `()` のみ。実行時は bool と同じく32bitスロットに常に0を格納する）
data Type = TyInt Signedness Width | TUsize | TBool | TUnit | TPtr Type | TArray Type Int deriving (Show, Eq)

-- 整数型の (符号の有無, レジスタ幅) への分解（usize は u64 と同じ表現）。整数型でなければ Nothing
intRepr :: Type -> Maybe (Signedness, Width)
intRepr (TyInt sg w) = Just (sg, w)
intRepr TUsize = Just (Unsigned, W64)
intRepr _ = Nothing

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
       in case matchIntSuffix rest of
            -- u64 の最大値のように Int の範囲を超える値は、read が同じビットパターンの負数へラップする
            Just (ty, rest') -> (TIntSuffixed (read digits) ty :) <$> tokenize rest'
            Nothing -> (TInt (read digits) :) <$> tokenize rest
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
            "as" -> (TAs :) <$> tokenize rest
            _ -> (TIdent ident :) <$> tokenize rest
  | c == '+' = (TPlus :) <$> tokenize cs
  | c == '-' = (TMinus :) <$> tokenize cs
  | c == '*' = (TStar :) <$> tokenize cs
  | c == '/' = (TSlash :) <$> tokenize cs
  | c == '&' = (TAmp :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | c == '{' = (TLBrace :) <$> tokenize cs
  | c == '}' = (TRBrace :) <$> tokenize cs
  | c == '[' = (TLBracket :) <$> tokenize cs
  | c == ']' = (TRBracket :) <$> tokenize cs
  | c == ',' = (TComma :) <$> tokenize cs
  | c == ':' = (TColon :) <$> tokenize cs
  | c == '=' = (TAssign :) <$> tokenize cs
  | c == '!' = (TBang :) <$> tokenize cs
  | c == '<' = (TLt :) <$> tokenize cs
  | c == '>' = (TGt :) <$> tokenize cs
  | c == ';' = (TSemicolon :) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])

-- 整数リテラル直後の型サフィックス（'i64'/'i32'/'u64'/'u32'/'usize'）を認識する。識別子境界（次の文字が
-- 識別子構成文字でない、または入力終端）まで含めて判定することで、`10i64`は`TIntSuffixed`に、
-- `10i64x`や`10i65`は誤ってサフィックスとして消費せず従来どおり`TInt`+`TIdent`のトークン列にする
matchIntSuffix :: String -> Maybe (Type, String)
matchIntSuffix s
  | Just rest <- stripPrefix "i64" s, boundary rest = Just (TyInt Signed W64, rest)
  | Just rest <- stripPrefix "i32" s, boundary rest = Just (TyInt Signed W32, rest)
  | Just rest <- stripPrefix "u64" s, boundary rest = Just (TyInt Unsigned W64, rest)
  | Just rest <- stripPrefix "u32" s, boundary rest = Just (TyInt Unsigned W32, rest)
  | Just rest <- stripPrefix "usize" s, boundary rest = Just (TUsize, rest)
  | otherwise = Nothing
 where
  boundary (ch : _) = not (isAlphaNum ch || ch == '_')
  boundary [] = True

-- Parser
--
-- program    ::= fn-decl*（main を含む。トップレベルに文・式は書けない）
-- fn-decl    ::= 'fn' IDENT '(' (IDENT ':' type (',' IDENT ':' type)*)? ')' ('->' type)? '{' stmt* expr? '}'
--              （末尾式を省略した場合は () とする）
-- stmt       ::= let-stmt | assign-stmt | place-assign-stmt | expr-stmt | block-stmt | if-stmt | while-stmt | break-stmt | continue-stmt | return-stmt
-- let-stmt    ::= 'let' IDENT ':' type '=' expr ';' | 'let' IDENT '=' expr ';'
-- assign-stmt ::= IDENT '=' expr ';'
-- place-assign-stmt ::= expr '=' expr ';'（左辺は場所式。a[i] / p[i] / (*p)[i] / *p など）
-- expr-stmt  ::= expr ';'
-- block-stmt ::= '{' stmt* '}'
-- if-stmt    ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?
-- while-stmt    ::= 'while' expr '{' stmt* '}'
-- break-stmt    ::= 'break' ';'
-- continue-stmt ::= 'continue' ';'
-- return-stmt   ::= 'return' expr? ';'（値を省略した場合は () とする）
-- type       ::= '&' type | '[' type ';' INT ']' | '(' ')' | 'i32' | 'i64' | 'u32' | 'u64' | 'usize' | 'bool'
--              ※ 配列の要素型に配列は不可（配列へのポインタ '&' '[' ... は可）
-- expr       ::= equality
-- equality   ::= comparison (('==' | '!=') comparison)*
-- comparison ::= additive (('<' | '<=' | '>' | '>=') additive)*
-- additive   ::= multiplicative (('+' | '-') multiplicative)*
-- multiplicative       ::= cast (('*' | '/') cast)*
-- cast       ::= factor ('as' type)*（type は整数型（'i32' | 'i64' | 'u32' | 'u64' | 'usize'）のみ）
-- factor     ::= '-' factor | '!' factor | '&' factor | '*' factor | postfix
-- postfix    ::= primary ('[' expr ']')*
-- primary    ::= INT | INT ('i64' | 'i32' | 'u64' | 'u32' | 'usize') | 'true' | 'false' | '(' ')' | IDENT | IDENT '(' (expr (',' expr)*)? ')'
--              | '(' expr ')' | '[' expr (',' expr)* ']' | '[' expr ';' INT ']'
--              ※ INT ('i64'|'i32'|'u64'|'u32'|'usize') は字句解析側で空白を挟まず1トークン（TIntSuffixed）に融合される

type ParseResult a = Either String (a, [Token])

-- program ::= fn-decl*
parse :: [Token] -> Either String Program
parse [] = Right []
parse (TFn : rest) = do
  (fnDecl, rest') <- parseFnDecl rest
  (fnDecl :) <$> parse rest'
parse (t : _) = Left ("expected 'fn' at top level, got: " ++ show t)

-- fn定義（先頭の'fn'は呼び出し側で消費済み）
parseFnDecl :: [Token] -> ParseResult FnDecl
parseFnDecl tokens = do
  (name, rest) <- expectIdent tokens
  rest' <- expectToken TLParen rest
  (params, rest'') <- parseParamList rest'
  -- 戻り値型の省略（'->' が無い）は () とする（Rustと同じ）
  (retTy, rest3) <- case rest'' of
    (TArrow : rest2) -> parseTypeAnnotation rest2
    _ -> Right (TUnit, rest'')
  rest5 <- expectToken TLBrace rest3
  (stmts, rest6) <- parseStmts rest5
  -- 末尾式の省略（直後が '}'）は () とする
  (tailExpr, rest7) <- case rest6 of
    (TRBrace : _) -> Right (UnitLit, rest6)
    _ -> parseExpr rest6
  case rest7 of
    (TRBrace : rest8) -> Right (FnDecl name params retTy (stmts, tailExpr), rest8)
    _ -> Left "expected closing brace"

-- 仮引数リスト: (IDENT ':' type (',' IDENT ':' type)*)?（'(' は呼び出し側で消費済み、終端の ')' はここで消費する）
parseParamList :: [Token] -> ParseResult [(String, Type)]
parseParamList (TRParen : rest) = Right ([], rest)
parseParamList tokens = do
  (param, rest) <- parseParam tokens
  parseParamListRest [param] rest

-- IDENT ':' type（型注釈は必須。シグネチャを宣言だけで確定させ、関数本体や呼び出し箇所を見ずに済むようにする）
parseParam :: [Token] -> ParseResult (String, Type)
parseParam tokens = do
  (name, rest) <- expectIdent tokens
  case rest of
    (TColon : rest') -> do
      (ty, rest'') <- parseTypeAnnotation rest'
      Right ((name, ty), rest'')
    _ -> Left ("expected ':' and type after parameter name: " ++ name)

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
-- キーワードで始まらない場合は式を試しに読み、次が '=' なら場所式への代入文、';' なら式文とする。
-- そうでなければ（末尾式 `a[0]` / `*p` など）文は無いものとして元のトークン列のまま戻る
-- （パースエラーも末尾式として読み直す側で報告される）
parseStmts tokens =
  case parseExpr tokens of
    Right (lhs, TAssign : rest) -> do
      (stmt, rest') <- parsePlaceAssignStmt lhs rest
      (stmts, rest'') <- parseStmts rest'
      Right (stmt : stmts, rest'')
    Right (e, TSemicolon : rest) -> do
      (stmts, rest') <- parseStmts rest
      Right (SExpr e : stmts, rest')
    _ -> Right ([], tokens)

-- let-stmt    ::= 'let' IDENT ':' type '=' expr ';' | 'let' IDENT '=' expr ';'
parseLetStmt :: [Token] -> ParseResult Stmt
parseLetStmt tokens = do
  (name, rest) <- expectIdent tokens
  case rest of
    (TColon : rest') -> do
      (ty, rest'') <- parseTypeAnnotation rest'
      rest3 <- expectToken TAssign rest''
      (expr, rest4) <- parseExpr rest3
      rest5 <- expectToken TSemicolon rest4
      Right (SLet name ty expr, rest5)
    -- 型注釈省略: 'let' IDENT '=' expr ';'（型はcompile側でinferTypeにより推論する）
    _ -> do
      rest' <- expectToken TAssign rest
      (expr, rest'') <- parseExpr rest'
      rest3 <- expectToken TSemicolon rest''
      Right (SLetInferred name expr, rest3)

-- 型名 -> Type の変換（ベース型のみ。'&'接頭辞は parseTypeAnnotation 側で処理する）
parseType :: String -> Either String Type
parseType "i32" = Right (TyInt Signed W32)
parseType "i64" = Right (TyInt Signed W64)
parseType "u32" = Right (TyInt Unsigned W32)
parseType "u64" = Right (TyInt Unsigned W64)
parseType "usize" = Right TUsize
parseType "bool" = Right TBool
parseType ty = Left ("unsupported type: " ++ ty)

-- type ::= '&' type | '[' type ';' INT ']' | '(' ')' | IDENT（IDENTは parseType でベース型に変換する）
parseTypeAnnotation :: [Token] -> ParseResult Type
parseTypeAnnotation (TLParen : TRParen : rest) = Right (TUnit, rest)
parseTypeAnnotation (TAmp : rest) = do
  (inner, rest') <- parseTypeAnnotation rest
  Right (TPtr inner, rest')
parseTypeAnnotation (TLBracket : rest) = do
  (elemTy, rest') <- parseTypeAnnotation rest
  case elemTy of
    TArray _ _ -> Left "nested arrays are not supported"
    _ -> Right ()
  rest'' <- expectToken TSemicolon rest'
  case rest'' of
    (TInt n : TRBracket : rest3)
      | n >= 1 -> Right (TArray elemTy n, rest3)
      | otherwise -> Left "array length must be at least 1"
    _ -> Left "expected array length (integer literal) followed by ']'"
parseTypeAnnotation tokens = do
  (tyName, rest) <- expectIdent tokens
  ty <- parseType tyName
  Right (ty, rest)

-- assign-stmt ::= IDENT '=' expr ';'
parseAssignStmt :: String -> [Token] -> ParseResult Stmt
parseAssignStmt name tokens = do
  (expr, rest) <- parseExpr tokens
  rest' <- expectToken TSemicolon rest
  Right (SAssign name expr, rest')

-- '[' の直後から expr ']' を読む（'[' は呼び出し側で消費済み、']' はここで消費する）
parseIndexTarget :: [Token] -> ParseResult Expr
parseIndexTarget tokens = do
  (idx, rest) <- parseExpr tokens
  rest' <- expectToken TRBracket rest
  Right (idx, rest')

-- place-assign-stmt ::= expr '=' expr ';'（左辺と '=' は呼び出し側で消費済み。左辺が場所式かどうかはcompile側で検査する）
parsePlaceAssignStmt :: Expr -> [Token] -> ParseResult Stmt
parsePlaceAssignStmt lhs tokens = do
  (expr, rest) <- parseExpr tokens
  rest' <- expectToken TSemicolon rest
  Right (SAssignPlace lhs expr, rest')

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

-- return-stmt ::= 'return' expr? ';'（先頭の'return'は呼び出し側で消費済み。値の省略は () とする）
parseReturnStmt :: [Token] -> ParseResult Stmt
parseReturnStmt (TSemicolon : rest) = Right (SReturn UnitLit, rest)
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
  (left, rest) <- parseCast tokens
  parseMultiplicativeRest left rest

-- キャストの抽出（単項演算子より低く、乗除算より高い優先順位。左結合で連鎖できる）
-- cast ::= factor ('as' type)*
parseCast :: [Token] -> ParseResult Expr
parseCast tokens = do
  (expr, rest) <- parseFactor tokens
  parseCastRest expr rest

parseCastRest :: Expr -> [Token] -> ParseResult Expr
parseCastRest expr (TAs : rest) = do
  (ty, rest') <- parseTypeAnnotation rest
  case intRepr ty of
    Just _ -> parseCastRest (Cast expr ty) rest'
    Nothing -> Left "invalid cast target: only integer types (i32/i64/u32/u64/usize) are allowed"
parseCastRest expr rest = Right (expr, rest)

-- multiplicative ::= cast (('*' | '/') cast)*
parseMultiplicativeRest :: Expr -> [Token] -> ParseResult Expr
parseMultiplicativeRest left (TStar : rest) = do
  (right, rest') <- parseCast rest
  parseMultiplicativeRest (Mul left right) rest'
parseMultiplicativeRest left (TSlash : rest) = do
  (right, rest') <- parseCast rest
  parseMultiplicativeRest (Div left right) rest'
parseMultiplicativeRest left rest = Right (left, rest)

-- factor ::= '-' factor | '!' factor | '&' factor | '*' factor | postfix
-- （添字 '[' ']' は後置演算子として単項演算子より強く結合する。`-a[0]` は `-(a[0])`、`&a[i]` は `&(a[i])`）
parseFactor :: [Token] -> ParseResult Expr
parseFactor (TMinus : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Neg expr, rest')
parseFactor (TBang : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Not expr, rest')
parseFactor (TAmp : rest) = do
  (expr, rest') <- parseFactor rest
  Right (AddrOf expr, rest')
parseFactor (TStar : rest) = do
  (expr, rest') <- parseFactor rest
  Right (Deref expr, rest')
parseFactor tokens = do
  (expr, rest) <- parsePrimary tokens
  parsePostfixRest expr rest

-- postfix ::= primary ('[' expr ']')*
parsePostfixRest :: Expr -> [Token] -> ParseResult Expr
parsePostfixRest expr (TLBracket : rest) = do
  (idx, rest') <- parseIndexTarget rest
  parsePostfixRest (Index expr idx) rest'
parsePostfixRest expr rest = Right (expr, rest)

-- primary ::= INT | INT ('i64' | 'i32' | 'u64' | 'u32' | 'usize') | 'true' | 'false' | '(' ')' | IDENT | IDENT '(' (expr (',' expr)*)? ')'
--           | '(' expr ')' | '[' expr (',' expr)* ']' | '[' expr ';' INT ']'
parsePrimary :: [Token] -> ParseResult Expr
parsePrimary (TInt n : rest) = Right (Lit n, rest)
parsePrimary (TIntSuffixed n ty : rest) = Right (LitTyped n ty, rest)
parsePrimary (TTrue : rest) = Right (BoolLit True, rest)
parsePrimary (TFalse : rest) = Right (BoolLit False, rest)
parsePrimary (TLParen : TRParen : rest) = Right (UnitLit, rest)
parsePrimary (TIdent name : TLParen : rest) = do
  (args, rest') <- parseArgList rest
  Right (Call name args, rest')
parsePrimary (TIdent name : rest) = Right (Var name, rest)
parsePrimary (TLParen : rest) = do
  (expr, rest') <- parseExpr rest
  case rest' of
    (TRParen : rest'') -> Right (expr, rest'')
    _ -> Left "expected closing parenthesis"
parsePrimary (TLBracket : TRBracket : _) = Left "empty array literal is not supported"
parsePrimary (TLBracket : rest) = do
  (first, rest') <- parseExpr rest
  case rest' of
    -- '[' expr ';' INT ']'（繰り返し回数は無型の整数リテラルのみ）
    (TSemicolon : TInt n : TRBracket : rest'')
      | n >= 1 -> Right (ArrayRepeat first n, rest'')
      | otherwise -> Left "array length must be at least 1"
    (TSemicolon : _) -> Left "expected array length (integer literal) followed by ']'"
    _ -> parseArrayElemsRest [first] rest'
parsePrimary [] = Left "unexpected end of input"
parsePrimary (t : _) = Left ("unexpected token: " ++ show t)

-- 配列リテラルの残りの要素: (',' expr)* ']'
parseArrayElemsRest :: [Expr] -> [Token] -> ParseResult Expr
parseArrayElemsRest acc (TComma : rest) = do
  (e, rest') <- parseExpr rest
  parseArrayElemsRest (acc ++ [e]) rest'
parseArrayElemsRest acc (TRBracket : rest) = Right (ArrayLit acc, rest)
parseArrayElemsRest _ (t : _) = Left ("expected ',' or ']', got: " ++ show t)
parseArrayElemsRest _ [] = Left "expected ',' or ']', got end of input"

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
