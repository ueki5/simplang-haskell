module Main where

import Compiler (Instr (..), compile, run)
import Data.Either (isLeft, isRight)
import Parser (Expr (..), FnDecl (..), Program, Signedness (..), Stmt (..), Token (..), Type (..), Width (..), parse, tokenize)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import CodeGen (codegen)

main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "整数を変換する" $
      tokenize "42" `shouldBe` Right [TInt 42]
    it "空白をスキップする" $
      tokenize "1 + 2" `shouldBe` Right [TInt 1, TPlus, TInt 2]
    it "全演算子を変換する" $
      tokenize "+-*/" `shouldBe` Right [TPlus, TMinus, TStar, TSlash]
    it "括弧を変換する" $
      tokenize "(1)" `shouldBe` Right [TLParen, TInt 1, TRParen]
    it "波括弧を変換する" $
      tokenize "{ 1 }" `shouldBe` Right [TLBrace, TInt 1, TRBrace]
    it "未知の文字はエラー" $
      tokenize "1 + @" `shouldBe` Left "unexpected character: @"
    it "識別子を変換する" $
      tokenize "abc" `shouldBe` Right [TIdent "abc"]
    it "let予約語を変換する" $
      tokenize "let" `shouldBe` Right [TLet]
    it "let予約語で始まる識別子はTIdentのまま" $
      tokenize "letter" `shouldBe` Right [TIdent "letter"]
    it "コロン・代入・セミコロンを変換する" $
      tokenize "x: i64 = 1;"
        `shouldBe` Right [TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon]
    it "==演算子を変換する" $
      tokenize "==" `shouldBe` Right [TEq]
    it "!=演算子を変換する" $
      tokenize "!=" `shouldBe` Right [TNeq]
    it "<演算子を変換する" $
      tokenize "<" `shouldBe` Right [TLt]
    it "<=演算子を変換する" $
      tokenize "<=" `shouldBe` Right [TLe]
    it ">演算子を変換する" $
      tokenize ">" `shouldBe` Right [TGt]
    it ">=演算子を変換する" $
      tokenize ">=" `shouldBe` Right [TGe]
    it "<=/>=は2文字トークンとして認識され、<+=や>+=に分割されない" $
      tokenize "<= >=" `shouldBe` Right [TLe, TGe]
    it "!演算子を変換する" $
      tokenize "!" `shouldBe` Right [TBang]
    it "=単体は引き続きTAssignになる（==との判別の回帰確認）" $
      tokenize "=" `shouldBe` Right [TAssign]
    it "true/false予約語を変換する" $
      tokenize "true false" `shouldBe` Right [TTrue, TFalse]
    it "true/falseで始まる識別子はTIdentのまま" $
      tokenize "truest falsely" `shouldBe` Right [TIdent "truest", TIdent "falsely"]
    it "as予約語を変換する" $
      tokenize "x as i64" `shouldBe` Right [TIdent "x", TAs, TIdent "i64"]
    it "asで始まる識別子はTIdentのまま" $
      tokenize "ask as_x" `shouldBe` Right [TIdent "ask", TIdent "as_x"]
    it "to_i64/to_i32は予約語ではなく識別子になる" $
      tokenize "to_i64 to_i32" `shouldBe` Right [TIdent "to_i64", TIdent "to_i32"]
    it "if/else予約語を変換する" $
      tokenize "if else" `shouldBe` Right [TIf, TElse]
    it "if/elseで始まる識別子はTIdentのまま" $
      tokenize "iffy elsewhere" `shouldBe` Right [TIdent "iffy", TIdent "elsewhere"]
    it "while/break/continue予約語を変換する" $
      tokenize "while break continue" `shouldBe` Right [TWhile, TBreak, TContinue]
    it "while/break/continueで始まる識別子はTIdentのまま" $
      tokenize "whiley breaker continued"
        `shouldBe` Right [TIdent "whiley", TIdent "breaker", TIdent "continued"]
    it "fn/return予約語を変換する" $
      tokenize "fn return" `shouldBe` Right [TFn, TReturn]
    it "fn/returnで始まる識別子はTIdentのまま" $
      tokenize "fnord returned" `shouldBe` Right [TIdent "fnord", TIdent "returned"]
    it "->演算子を変換する" $
      tokenize "->" `shouldBe` Right [TArrow]
    it "->は2文字トークンとして認識され、-と>に分割されない" $
      tokenize "a -> b" `shouldBe` Right [TIdent "a", TArrow, TIdent "b"]
    it "-単体は引き続きTMinusになる（->との判別の回帰確認）" $
      tokenize "1 - 2" `shouldBe` Right [TInt 1, TMinus, TInt 2]
    it "カンマを変換する" $
      tokenize "a, b" `shouldBe` Right [TIdent "a", TComma, TIdent "b"]
    it "&演算子を変換する" $
      tokenize "&" `shouldBe` Right [TAmp]
    it "&で始まる識別子とは別トークンとして分割される" $
      tokenize "&a" `shouldBe` Right [TAmp, TIdent "a"]
    it "&&は2つのTAmpとして分割される（論理積演算子は存在しない）" $
      tokenize "&&i64" `shouldBe` Right [TAmp, TAmp, TIdent "i64"]
    it "i64サフィックス付き整数リテラルを変換する" $
      tokenize "9999i64" `shouldBe` Right [TIntSuffixed 9999 (TyInt Signed W64)]
    it "i32サフィックス付き整数リテラルを変換する" $
      tokenize "123i32" `shouldBe` Right [TIntSuffixed 123 (TyInt Signed W32)]
    it "サフィックス付き整数リテラルは空白を挟まなくても後続トークンと分割される" $
      tokenize "9999i64+1" `shouldBe` Right [TIntSuffixed 9999 (TyInt Signed W64), TPlus, TInt 1]
    it "u32/u64サフィックス付き整数リテラルを変換する" $
      tokenize "5u32 7u64" `shouldBe` Right [TIntSuffixed 5 (TyInt Unsigned W32), TIntSuffixed 7 (TyInt Unsigned W64)]
    it "u64の最大値はInt上で同じビットパターン（-1）として保持する" $
      tokenize "18446744073709551615u64" `shouldBe` Right [TIntSuffixed (-1) (TyInt Unsigned W64)]
    it "u32の後に識別子文字が続く場合はサフィックスとみなさない" $
      tokenize "5u32x" `shouldBe` Right [TInt 5, TIdent "u32x"]
    it "数字の直後が'i64'/'i32'に一致しない識別子ならサフィックスとして扱わずTInt+TIdentのまま" $
      tokenize "10abc" `shouldBe` Right [TInt 10, TIdent "abc"]
    it "'i64'/'i32'の直後に識別子構成文字が続く場合はサフィックスとして誤消費しない" $
      tokenize "10i64x" `shouldBe` Right [TInt 10, TIdent "i64x"]

  describe "parse" $ do
    it "整数リテラル" $
      parseMain [TInt 5] `shouldBe` Right ([], [], Lit 5)
    it "i64サフィックス付き整数リテラル" $
      parseMain [TIntSuffixed 9999 (TyInt Signed W64)] `shouldBe` Right ([], [], LitTyped 9999 (TyInt Signed W64))
    it "i32サフィックス付き整数リテラル" $
      parseMain [TIntSuffixed 123 (TyInt Signed W32)] `shouldBe` Right ([], [], LitTyped 123 (TyInt Signed W32))
    it "サフィックス付き整数リテラルの加算" $
      parseMain [TIntSuffixed 1 (TyInt Signed W64), TPlus, TInt 2]
        `shouldBe` Right ([], [], Add (LitTyped 1 (TyInt Signed W64)) (Lit 2))
    it "加算" $
      parseMain [TInt 1, TPlus, TInt 2] `shouldBe` Right ([], [], Add (Lit 1) (Lit 2))
    it "減算" $
      parseMain [TInt 3, TMinus, TInt 1] `shouldBe` Right ([], [], Sub (Lit 3) (Lit 1))
    it "乗算が加算より優先される" $
      parseMain [TInt 1, TPlus, TInt 2, TStar, TInt 3]
        `shouldBe` Right ([], [], Add (Lit 1) (Mul (Lit 2) (Lit 3)))
    it "括弧で優先度を変える" $
      parseMain [TLParen, TInt 1, TPlus, TInt 2, TRParen, TStar, TInt 3]
        `shouldBe` Right ([], [], Mul (Add (Lit 1) (Lit 2)) (Lit 3))
    it "単項マイナス" $
      parseMain [TMinus, TInt 5] `shouldBe` Right ([], [], Neg (Lit 5))
    it "空のmain本体は末尾式 ()" $
      parseMain [] `shouldBe` Right ([], [], UnitLit)
    it "変数参照" $
      parseMain [TIdent "x"] `shouldBe` Right ([], [], Var "x")
    it "let宣言（i64）" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" (TyInt Signed W64) (Lit 1)], Var "x")
    it "let宣言（i32）" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i32", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" (TyInt Signed W32) (Lit 1)], Var "x")
    it "let宣言（bool）" $
      parseMain [TLet, TIdent "x", TColon, TIdent "bool", TAssign, TTrue, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" TBool (BoolLit True)], Var "x")
    it "let宣言（&i64、アドレス取得）" $
      parseMain
        [ TLet, TIdent "a", TColon, TIdent "i64", TAssign, TInt 0, TSemicolon
        , TLet, TIdent "b", TColon, TAmp, TIdent "i64", TAssign, TAmp, TIdent "a", TSemicolon
        , TIdent "b"
        ]
        `shouldBe` Right
          ( []
          , [SLet "a" (TyInt Signed W64) (Lit 0), SLet "b" (TPtr (TyInt Signed W64)) (AddrOf (Var "a"))]
          , Var "b"
          )
    it "let宣言（&&i64、ポインタのポインタ）" $
      parseMain [TLet, TIdent "b", TColon, TAmp, TAmp, TIdent "i64", TAssign, TIdent "b", TSemicolon, TIdent "b"]
        `shouldBe` Right ([], [SLet "b" (TPtr (TPtr (TyInt Signed W64))) (Var "b")], Var "b")
    it "let宣言（&bool）" $
      parseMain [TLet, TIdent "b", TColon, TAmp, TIdent "bool", TAssign, TIdent "b", TSemicolon, TIdent "b"]
        `shouldBe` Right ([], [SLet "b" (TPtr TBool) (Var "b")], Var "b")
    it "デリファレンス（*式）" $
      parseMain [TStar, TIdent "b"] `shouldBe` Right ([], [], Deref (Var "b"))
    it "二重デリファレンス（**式）" $
      parseMain [TStar, TStar, TIdent "p"] `shouldBe` Right ([], [], Deref (Deref (Var "p")))
    it "アドレス取得と乗算は位置で判別される（回帰確認）: 1 * 2 は不変" $
      parseMain [TInt 1, TStar, TInt 2] `shouldBe` Right ([], [], Mul (Lit 1) (Lit 2))
    it "被乗数側のデリファレンス: a * *b" $
      parseMain [TIdent "a", TStar, TStar, TIdent "b"]
        `shouldBe` Right ([], [], Mul (Var "a") (Deref (Var "b")))
    it "乗数側のデリファレンス: *a * b" $
      parseMain [TStar, TIdent "a", TStar, TIdent "b"]
        `shouldBe` Right ([], [], Mul (Deref (Var "a")) (Var "b"))
    it "アドレス取得演算子（&式）" $
      parseMain [TAmp, TIdent "a"] `shouldBe` Right ([], [], AddrOf (Var "a"))
    it "アドレス取得したポインタのデリファレンス（&*p）" $
      parseMain [TAmp, TStar, TIdent "p"] `shouldBe` Right ([], [], AddrOf (Deref (Var "p")))
    it "代入文" $
      parseMain
        [ TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TIdent "x", TAssign, TInt 2, TSemicolon
        , TIdent "x"
        ]
        `shouldBe` Right ([], [SLet "x" (TyInt Signed W64) (Lit 1), SAssign "x" (Lit 2)], Var "x")
    it "let宣言（型注釈省略）: コロンが無ければSLetInferredとしてパースする" $
      parseMain [TLet, TIdent "x", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLetInferred "x" (Lit 1)], Var "x")
    it "let宣言（型注釈省略、サフィックス付きリテラル）" $
      parseMain [TLet, TIdent "x", TAssign, TIntSuffixed 1 (TyInt Signed W32), TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLetInferred "x" (LitTyped 1 (TyInt Signed W32))], Var "x")
    it "let宣言（型注釈あり・省略）が混在しても両方正しくパースされる" $
      parseMain
        [ TLet, TIdent "a", TColon, TIdent "i32", TAssign, TInt 1, TSemicolon
        , TLet, TIdent "b", TAssign, TIdent "a", TSemicolon
        , TIdent "b"
        ]
        `shouldBe` Right
          ([], [SLet "a" (TyInt Signed W32) (Lit 1), SLetInferred "b" (Var "a")], Var "b")
    it "let宣言（型注釈省略）で=が無ければエラー" $
      parseMain [TLet, TIdent "x", TInt 1, TSemicolon, TIdent "x"] `shouldSatisfy` isLeft
    it "let宣言で型がi32/i64/bool以外ならエラー" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i16", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Left "unsupported type: i16"
    it "let宣言で=が無ければエラー" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i64", TInt 1, TSemicolon, TIdent "x"]
        `shouldSatisfy` isLeft
    it "文の終端に;が無ければエラー" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TIdent "x"]
        `shouldSatisfy` isLeft
    it "文だけのmain本体は末尾式 () に脱糖する" $
      parseMain [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon]
        `shouldBe` Right ([], [SLet "x" (TyInt Signed W64) (Lit 1)], UnitLit)
    it "末尾式の後に;を書くと式文になる" $
      parseMain [TInt 1, TSemicolon] `shouldBe` Right ([], [SExpr (Lit 1)], UnitLit)
    it "boolリテラル（true）" $
      parseMain [TTrue] `shouldBe` Right ([], [], BoolLit True)
    it "boolリテラル（false）" $
      parseMain [TFalse] `shouldBe` Right ([], [], BoolLit False)
    it "等価演算子" $
      parseMain [TInt 1, TEq, TInt 2] `shouldBe` Right ([], [], Eq (Lit 1) (Lit 2))
    it "非等価演算子" $
      parseMain [TInt 1, TNeq, TInt 2] `shouldBe` Right ([], [], Neq (Lit 1) (Lit 2))
    it "小なり演算子" $
      parseMain [TInt 1, TLt, TInt 2] `shouldBe` Right ([], [], Lt (Lit 1) (Lit 2))
    it "以下演算子" $
      parseMain [TInt 1, TLe, TInt 2] `shouldBe` Right ([], [], Le (Lit 1) (Lit 2))
    it "大なり演算子" $
      parseMain [TInt 1, TGt, TInt 2] `shouldBe` Right ([], [], Gt (Lit 1) (Lit 2))
    it "以上演算子" $
      parseMain [TInt 1, TGe, TInt 2] `shouldBe` Right ([], [], Ge (Lit 1) (Lit 2))
    it "比較演算子は加減算より優先順位が低い" $
      parseMain [TInt 1, TPlus, TInt 2, TLt, TInt 3]
        `shouldBe` Right ([], [], Lt (Add (Lit 1) (Lit 2)) (Lit 3))
    it "比較演算子は等価演算子より強く結合する" $
      parseMain [TInt 1, TLt, TInt 2, TEq, TTrue]
        `shouldBe` Right ([], [], Eq (Lt (Lit 1) (Lit 2)) (BoolLit True))
    it "括弧内の比較演算子をパースできる" $
      parseMain [TLParen, TInt 1, TLt, TInt 2, TRParen]
        `shouldBe` Right ([], [], Lt (Lit 1) (Lit 2))
    it "否定演算子" $
      parseMain [TBang, TTrue] `shouldBe` Right ([], [], Not (BoolLit True))
    it "等価演算子は加減算より優先順位が低い" $
      parseMain [TInt 1, TPlus, TInt 2, TEq, TInt 3]
        `shouldBe` Right ([], [], Eq (Add (Lit 1) (Lit 2)) (Lit 3))
    it "否定演算子は等価演算子より強く結合する" $
      parseMain [TBang, TTrue, TEq, TFalse]
        `shouldBe` Right ([], [], Eq (Not (BoolLit True)) (BoolLit False))
    it "括弧内の等価演算子をパースできる" $
      parseMain [TLParen, TInt 1, TEq, TInt 1, TRParen]
        `shouldBe` Right ([], [], Eq (Lit 1) (Lit 1))
    it "as i64キャスト" $
      parseMain [TIdent "x", TAs, TIdent "i64"] `shouldBe` Right ([], [], Cast (Var "x") (TyInt Signed W64))
    it "as i32キャスト" $
      parseMain [TInt 64, TAs, TIdent "i32"] `shouldBe` Right ([], [], Cast (Lit 64) (TyInt Signed W32))
    it "キャストは左結合で連鎖できる" $
      parseMain [TIdent "x", TAs, TIdent "i32", TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Cast (Cast (Var "x") (TyInt Signed W32)) (TyInt Signed W64))
    it "単項マイナスはasより強く結合する" $
      parseMain [TMinus, TIdent "x", TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Cast (Neg (Var "x")) (TyInt Signed W64))
    it "デリファレンスはasより強く結合する" $
      parseMain [TStar, TIdent "p", TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Cast (Deref (Var "p")) (TyInt Signed W64))
    it "asは乗算より強く結合する" $
      parseMain [TIdent "a", TStar, TIdent "b", TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Mul (Var "a") (Cast (Var "b") (TyInt Signed W64)))
    it "asは加算より強く結合する" $
      parseMain [TIdent "a", TPlus, TIdent "b", TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Add (Var "a") (Cast (Var "b") (TyInt Signed W64)))
    it "括弧で囲んだ式をキャストできる" $
      parseMain [TLParen, TIdent "a", TPlus, TIdent "b", TRParen, TAs, TIdent "i64"]
        `shouldBe` Right ([], [], Cast (Add (Var "a") (Var "b")) (TyInt Signed W64))
    it "boolへのキャストはエラー" $
      parseMain [TIdent "x", TAs, TIdent "bool"]
        `shouldBe` Left "invalid cast target: only integer types (i32/i64/u32/u64/usize) are allowed"
    it "ポインタ型へのキャストはエラー" $
      parseMain [TIdent "x", TAs, TAmp, TIdent "i64"]
        `shouldBe` Left "invalid cast target: only integer types (i32/i64/u32/u64/usize) are allowed"
    it "未知の型へのキャストはエラー" $
      parseMain [TIdent "x", TAs, TIdent "foo"] `shouldBe` Left "unsupported type: foo"
    it "as u32キャスト" $
      parseMain [TIdent "x", TAs, TIdent "u32"] `shouldBe` Right ([], [], Cast (Var "x") (TyInt Unsigned W32))
    it "as u64キャスト" $
      parseMain [TIdent "x", TAs, TIdent "u64"] `shouldBe` Right ([], [], Cast (Var "x") (TyInt Unsigned W64))
    it "u32/u64の型注釈付きlet" $
      parseMain [TLet, TIdent "x", TColon, TIdent "u32", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" (TyInt Unsigned W32) (Lit 1)], Var "x")
    it "u32サフィックス付きリテラルをパースする" $
      parseMain [TIntSuffixed 5 (TyInt Unsigned W32)] `shouldBe` Right ([], [], LitTyped 5 (TyInt Unsigned W32))
    it "asの後に型が無いとエラー" $
      parseMain [TIdent "x", TAs] `shouldSatisfy` isLeft
    it "空ブロック" $
      parseMain [TLBrace, TRBrace, TInt 5]
        `shouldBe` Right ([], [SBlock []], Lit 5)
    it "ブロック内にlet文を含む" $
      parseMain
        [ TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SBlock [SLet "x" (TyInt Signed W64) (Lit 1)]], Lit 2)
    it "ネストしたブロック" $
      parseMain [TLBrace, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SBlock []]], Lit 1)
    it "閉じ括弧がないブロックはエラー" $
      parse [TFn, TIdent "main", TLParen, TRParen, TLBrace, TLBrace] `shouldBe` Left "expected closing brace"
    it "if文（elseなし）" $
      parseMain [TIf, TTrue, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SIf [(BoolLit True, [])] Nothing], Lit 1)
    it "if-else文" $
      parseMain [TIf, TTrue, TLBrace, TRBrace, TElse, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SIf [(BoolLit True, [])] (Just [])], Lit 1)
    it "if-else if-else文（複数のelse if）" $
      parseMain
        [ TIf, TTrue, TLBrace, TRBrace
        , TElse, TIf, TFalse, TLBrace, TRBrace
        , TElse, TIf, TTrue, TLBrace, TRBrace
        , TElse, TLBrace, TRBrace
        , TInt 1
        ]
        `shouldBe` Right
          ( []
          , [SIf [(BoolLit True, []), (BoolLit False, []), (BoolLit True, [])] (Just [])]
          , Lit 1
          )
    it "if文の本体にlet文を含む" $
      parseMain
        [ TIf, TTrue, TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SIf [(BoolLit True, [SLet "x" (TyInt Signed W64) (Lit 1)])] Nothing], Lit 2)
    it "if文はブロック文としてネストできる" $
      parseMain [TLBrace, TIf, TTrue, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SIf [(BoolLit True, [])] Nothing]], Lit 1)
    it "if文の条件式が無ければエラー" $
      parseMain [TIf, TLBrace, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "if文で閉じ波括弧がなければエラー" $
      parseMain [TIf, TTrue, TLBrace, TInt 1] `shouldBe` Left "expected closing brace"
    it "elseの後にifも{もなければエラー" $
      parseMain [TIf, TTrue, TLBrace, TRBrace, TElse, TInt 1] `shouldSatisfy` isLeft
    it "elseで入力が終わるとエラー" $
      parseMain [TIf, TTrue, TLBrace, TRBrace, TElse] `shouldSatisfy` isLeft
    it "while文" $
      parseMain [TWhile, TTrue, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SWhile (BoolLit True) []], Lit 1)
    it "while文の本体にlet文とbreak文を含む" $
      parseMain
        [ TWhile, TTrue, TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TBreak, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SWhile (BoolLit True) [SLet "x" (TyInt Signed W64) (Lit 1), SBreak]], Lit 2)
    it "continue文" $
      parseMain [TWhile, TTrue, TLBrace, TContinue, TSemicolon, TRBrace, TInt 1]
        `shouldBe` Right ([], [SWhile (BoolLit True) [SContinue]], Lit 1)
    it "while文はブロック文としてネストできる" $
      parseMain [TLBrace, TWhile, TTrue, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SWhile (BoolLit True) []]], Lit 1)
    it "while文の条件式が無ければエラー" $
      parseMain [TWhile, TLBrace, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "while文で閉じ波括弧がなければエラー" $
      parseMain [TWhile, TTrue, TLBrace, TInt 1] `shouldBe` Left "expected closing brace"
    it "break文に;が無ければエラー" $
      parseMain [TWhile, TTrue, TLBrace, TBreak, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "continue文に;が無ければエラー" $
      parseMain [TWhile, TTrue, TLBrace, TContinue, TRBrace, TInt 1] `shouldSatisfy` isLeft

  describe "parse（fn定義・呼び出し・return）" $ do
    let parseSrc src = tokenize src >>= fmap splitMain . parse
    it "引数無しのfn定義" $
      parseSrc "fn f() -> i64 {\n1\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [] (TyInt Signed W64) ([], Lit 1)], [], Lit 2)
    it "引数1個のfn定義" $
      parseSrc "fn f(a: i64) -> i64 {\na\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [("a", TyInt Signed W64)] (TyInt Signed W64) ([], Var "a")], [], Lit 2)
    it "引数複数個のfn定義（型が混在してもパースできる。型検査は別）" $
      parseSrc "fn f(a: i64, b: i32, c: bool) -> i64 {\na\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right
          ( [ FnDecl
                "f"
                [("a", TyInt Signed W64), ("b", TyInt Signed W32), ("c", TBool)]
                (TyInt Signed W64)
                ([], Var "a")
            ]
          , []
          , Lit 2
          )
    it "fn本体は文の列＋末尾式を持てる" $
      parseSrc "fn f() -> i64 {\nlet x: i64 = 1;\nx\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt Signed W64) ([SLet "x" (TyInt Signed W64) (Lit 1)], Var "x")]
          , []
          , Lit 2
          )
    it "fn定義は複数並べられる" $
      parseSrc "fn f() -> i64 {\n1\n}\nfn g() -> i64 {\n2\n}\nfn main() -> i64 {\n3\n}"
        `shouldBe` Right
          ( [ FnDecl "f" [] (TyInt Signed W64) ([], Lit 1)
            , FnDecl "g" [] (TyInt Signed W64) ([], Lit 2)
            ]
          , []
          , Lit 3
          )
    it "fn定義はmainの前後どちらにも書ける" $
      parseSrc "fn main() -> i64 {\nlet a: i64 = 1;\nlet b: i64 = 2;\na + b\n}\nfn f() -> i64 {\n1\n}"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt Signed W64) ([], Lit 1)]
          , [SLet "a" (TyInt Signed W64) (Lit 1), SLet "b" (TyInt Signed W64) (Lit 2)]
          , Add (Var "a") (Var "b")
          )
    it "fn本体の末尾式を省略すると () に脱糖する（型の不一致はcompile側で検出）" $
      parseSrc "fn f() -> i64 {\nlet x: i64 = 1;\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [] (TyInt Signed W64) ([SLet "x" (TyInt Signed W64) (Lit 1)], UnitLit)], [], Lit 2)
    it "パラメータの型注釈は省略できない" $
      parseSrc "fn add(a, b: i64) -> i64 {\na + b\n}\nfn main() -> i64 {\n0\n}"
        `shouldBe` Left "expected ':' and type after parameter name: a"
    it "戻り値型を省略すると () とする（Rustと同じ）" $
      parseSrc "fn f(a: i64) {\n}\nfn main() -> i64 {\n0\n}"
        `shouldBe` Right ([FnDecl "f" [("a", TyInt Signed W64)] TUnit ([], UnitLit)], [], Lit 0)
    it "-> () の明示は戻り値型の省略と同じ" $
      parseSrc "fn f(a: i64) -> () {\n}\nfn main() -> i64 {\n0\n}"
        `shouldBe` Right ([FnDecl "f" [("a", TyInt Signed W64)] TUnit ([], UnitLit)], [], Lit 0)
    it "引数無しの呼び出し式" $
      parseSrc "fn main() {\nf()\n}" `shouldBe` Right ([], [], Call "f" [])
    it "引数1個の呼び出し式" $
      parseSrc "fn main() {\nf(1)\n}" `shouldBe` Right ([], [], Call "f" [Lit 1])
    it "引数複数個の呼び出し式（カンマ区切り）" $
      parseSrc "fn main() {\nf(1, 2 + 3, x)\n}" `shouldBe` Right ([], [], Call "f" [Lit 1, Add (Lit 2) (Lit 3), Var "x"])
    it "呼び出し式は算術式の中で使える" $
      parseSrc "fn main() {\n1 + f(2)\n}" `shouldBe` Right ([], [], Add (Lit 1) (Call "f" [Lit 2]))
    it "呼び出し式は入れ子にできる" $
      parseSrc "fn main() {\nf(g(1))\n}" `shouldBe` Right ([], [], Call "f" [Call "g" [Lit 1]])
    it "return文（値付き）" $
      parseSrc "fn f() -> i64 {\nreturn 1;\n2\n}\nfn main() -> i64 {\n3\n}"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt Signed W64) ([SReturn (Lit 1)], Lit 2)]
          , []
          , Lit 3
          )
    it "値の無いreturn文は return () に脱糖する（型の不一致はcompile側で検出）" $
      parseSrc "fn f() -> i64 {\nreturn;\n1\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [] (TyInt Signed W64) ([SReturn UnitLit], Lit 1)], [], Lit 2)
    it "return文に;が無ければエラー" $
      parseSrc "fn f() -> i64 {\nreturn 1\n2\n}\nfn main() {\n3\n}" `shouldSatisfy` isLeft

  describe "codegen" $ do
    it "プロローグにmainラベルを含む" $
      codegen [] (TyInt Signed W64) [] `shouldContain` "main:"
    it "Push nをmovabsq+pushq命令に変換する" $
      codegen [] (TyInt Signed W64) [Push 42] `shouldContain` "movabsq $42, %rax"
    it "IAdd W64をaddq命令に変換する" $
      codegen [] (TyInt Signed W64) [IAdd W64] `shouldContain` "addq"
    it "ISub W64をsubq命令に変換する" $
      codegen [] (TyInt Signed W64) [ISub W64] `shouldContain` "subq"
    it "IMul W64をimulq命令に変換する" $
      codegen [] (TyInt Signed W64) [IMul W64] `shouldContain` "imulq"
    it "IDiv W64をidivq命令に変換する" $
      codegen [] (TyInt Signed W64) [IDiv W64] `shouldContain` "idivq"
    it "INeg W64をnegq命令に変換する" $
      codegen [] (TyInt Signed W64) [INeg W64] `shouldContain` "negq"
    it "IAdd W32をaddl命令に変換する" $
      codegen [] (TyInt Signed W32) [IAdd W32] `shouldContain` "addl"
    it "ISub W32をsubl命令に変換する" $
      codegen [] (TyInt Signed W32) [ISub W32] `shouldContain` "subl"
    it "IMul W32をimull命令に変換する" $
      codegen [] (TyInt Signed W32) [IMul W32] `shouldContain` "imull"
    it "IDiv W32をidivl命令に変換する" $
      codegen [] (TyInt Signed W32) [IDiv W32] `shouldContain` "idivl"
    it "INeg W32をnegl命令に変換する" $
      codegen [] (TyInt Signed W32) [INeg W32] `shouldContain` "negl"
    it "IDivにゼロ除算チェックを含む" $
      codegen [] (TyInt Signed W64) [IDiv W64] `shouldContain` ".Ldiv_zero_error"
    it "IDiv W64は除数-1の場合に被除数を符号反転する（cmoveq）" $
      codegen [] (TyInt Signed W64) [IDiv W64] `shouldContain` "cmoveq"
    it "IDiv W32は除数-1の場合に被除数を符号反転する（cmovel）" $
      codegen [] (TyInt Signed W32) [IDiv W32] `shouldContain` "cmovel"
    it "IZext32をmovl %eax, %eaxに変換する（u32のゼロ拡張正規化）" $
      codegen [] (TyInt Unsigned W32) [Push 1, IZext32] `shouldContain` "movl  %eax, %eax"
    it "IDivU W32はdivlで符号なし除算する" $
      codegen [] (TyInt Unsigned W32) [IDivU W32] `shouldContain` "divl  %ecx"
    it "IDivU W64はdivqで符号なし除算する" $
      codegen [] (TyInt Unsigned W64) [IDivU W64] `shouldContain` "divq  %rcx"
    it "ICmpLtUはsetbを使う" $
      codegen [] TBool [ICmpLtU] `shouldContain` "setb  %al"
    it "ICmpGeUはsetaeを使う" $
      codegen [] TBool [ICmpGeU] `shouldContain` "setae %al"
    it "最終値がu32なら%uフォーマットを使う" $
      codegen [] (TyInt Unsigned W32) [] `shouldContain` "fmtU32(%rip)"
    it "最終値がu64なら%luフォーマットを使う" $
      codegen [] (TyInt Unsigned W64) [] `shouldContain` "fmtU64(%rip)"
    it "エピローグにprintf呼び出しを含む" $
      codegen [] (TyInt Signed W64) [] `shouldContain` "call  printf"
    it "最終値がi64なら%ldフォーマットを使う" $
      codegen [] (TyInt Signed W64) [] `shouldContain` "\"%ld\\n\""
    it "最終値がi32なら%dフォーマットを使う" $
      codegen [] (TyInt Signed W32) [] `shouldContain` "\"%d\\n\""
    it "Load W64をmovq+pushqに変換する" $
      codegen [] (TyInt Signed W64) [Load W64 (-8)] `shouldContain` "movq  -8(%rbp), %rax"
    it "Store W64をpopq+movqに変換する" $
      codegen [] (TyInt Signed W64) [Store W64 (-8)] `shouldContain` "movq  %rax, -8(%rbp)"
    it "Load W32をmovslq+pushqに変換する（符号拡張ロード）" $
      codegen [] (TyInt Signed W32) [Load W32 (-4)] `shouldContain` "movslq -4(%rbp), %rax"
    it "Store W32をpopq+movlに変換する（切り詰めストア）" $
      codegen [] (TyInt Signed W32) [Store W32 (-4)] `shouldContain` "movl  %eax, -4(%rbp)"
    it "変数がある場合はsubqでスタックフレームを確保する（16バイト境界へ切り上げ）" $
      codegen [] (TyInt Signed W64) [Store W64 (-8)] `shouldContain` "subq  $16, %rsp"
    it "変数が無ければsubqを出さない" $
      codegen [] (TyInt Signed W64) [] `shouldNotContain` "subq"
    it "printf呼び出し前にスタックアライメントを揃える" $
      codegen [] (TyInt Signed W64) [] `shouldContain` "andq  $-16, %rsp"
    it "エピローグはleave命令を使う" $
      codegen [] (TyInt Signed W64) [] `shouldContain` "leave"
    it "ICmpEqをcmpq+sete命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpEq] `shouldContain` "sete"
    it "ICmpNeをcmpq+setne命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpNe] `shouldContain` "setne"
    it "ICmpLtをcmpq+setl命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpLt] `shouldContain` "setl"
    it "ICmpLeをcmpq+setle命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpLe] `shouldContain` "setle"
    it "ICmpGtをcmpq+setg命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpGt] `shouldContain` "setg"
    it "ICmpGeをcmpq+setge命令に変換する" $
      codegen [] (TyInt Signed W64) [ICmpGe] `shouldContain` "setge"
    it "INotをxorq命令に変換する" $
      codegen [] (TyInt Signed W64) [INot] `shouldContain` "xorq"
    it "ISext32をcltq命令に変換する（as i64/as i32共通の符号拡張正規化）" $
      codegen [] (TyInt Signed W64) [ISext32] `shouldContain` "cltq"
    it "JmpIfZeroはpop+testq+je命令に変換する" $ do
      let asm = codegen [] (TyInt Signed W64) [JmpIfZero ".Lfoo"]
      asm `shouldContain` "testq %rax, %rax"
      asm `shouldContain` "je    .Lfoo"
    it "Jmpはjmp命令に変換する" $
      codegen [] (TyInt Signed W64) [Jmp ".Lfoo"] `shouldContain` "jmp   .Lfoo"
    it "Labelはラベル定義行に変換する" $
      codegen [] (TyInt Signed W64) [Label ".Lfoo"] `shouldContain` ".Lfoo:"
    it "最終値がboolなら分岐でtrue/false文字列を出力する" $ do
      let asm = codegen [] TBool []
      asm `shouldContain` "testq %rax, %rax"
      asm `shouldContain` "\"true\\n\""
      asm `shouldContain` "\"false\\n\""
    it "LoadAddrをleaq+pushqに変換する（実効アドレス計算）" $
      codegen [] (TyInt Signed W64) [LoadAddr (-8)] `shouldContain` "leaq  -8(%rbp), %rax"
    it "LoadInd W64をレジスタ間接movqに変換する" $
      codegen [] (TyInt Signed W64) [LoadInd W64] `shouldContain` "movq  (%rax), %rax"
    it "LoadInd W32をレジスタ間接movslqに変換する（符号拡張ロード）" $
      codegen [] (TyInt Signed W32) [LoadInd W32] `shouldContain` "movslq (%rax), %rax"
    it "最終値がポインタ型なら%pフォーマットを使う" $ do
      let asm = codegen [] (TPtr (TyInt Signed W64)) []
      asm `shouldContain` "fmtPtr"
      asm `shouldContain` "\"%p\\n\""

  describe "意味論エラー（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "let初期化式内の未宣言参照はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = y;\nx\n}" `shouldBe` Left "undeclared variable: y"
    it "let初期化式内の自己参照はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = x + 1;\nx\n}" `shouldBe` Left "undeclared variable: x"
    it "代入文右辺の未宣言参照はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\nx = y + 1;\nx\n}" `shouldBe` Left "undeclared variable: y"
    it "末尾式での未宣言参照はエラー" $
      compileSource "fn main() {\nlet x: i64 = 1;\ny\n}" `shouldBe` Left "undeclared variable: y"
    it "再宣言はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\nlet x: i64 = 2;\nx\n}"
        `shouldBe` Left "variable already declared: x"
    it "型注釈がi32/i64/bool以外はエラー" $
      compileSource "fn main() {\nlet x: i16 = 1;\nx\n}" `shouldBe` Left "unsupported type: i16"
    it "let初期化式でi32変数とi64変数を混在させるとエラー" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nlet y: i64 = 2;\nlet z: i64 = x + y;\nz\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "代入文右辺でi32変数とi64変数を混在させるとエラー" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nlet y: i64 = 2;\ny = x + 1;\ny\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "式文でi32変数とi64変数を混在させるとエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\nlet y: i64 = 2;\nx + y;\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "宣言した型と異なる型の変数を代入するとエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nlet y: i64 = 2;\nlet z: i32 = y;\nz\n}"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "bool変数に整数リテラルを代入するとエラー" $
      compileSource "fn main() -> bool {\nlet x: bool = 1;\nx\n}"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "整数変数にboolリテラルを代入するとエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = true;\nx\n}"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "i64サフィックス付きリテラル単体はi64としてコンパイルされる" $
      compileSource "fn main() -> i64 {\n9999i64\n}" `shouldBe` Right ([], TyInt Signed W64, [Push 9999, Label ".Lfn_end0"])
    it "i32サフィックス付きリテラル単体はi32としてコンパイルされる" $
      compileSource "fn main() -> i32 {\n9999i32\n}" `shouldBe` Right ([], TyInt Signed W32, [Push 9999, Label ".Lfn_end0"])
    it "i64サフィックス付きリテラルをi32変数へ代入すると幅不一致でエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 9999i64;\nx\n}"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "i32サフィックス付きリテラルをi64変数へ代入すると幅不一致でエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 9999i32;\nx\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "サフィックス付きリテラルをbool変数へ代入するとエラー" $
      compileSource "fn main() -> bool {\nlet x: bool = 9999i64;\nx\n}"
        `shouldBe` Left "type mismatch: expected bool, found i64"
    it "無型リテラルとの混在ではサフィックス側の型に固定される" $
      compileSource "fn main() -> i32 {\n9999i32 + 5\n}" `shouldBe` Right ([], TyInt Signed W32, [Push 9999, Push 5, IAdd W32, Label ".Lfn_end0"])
    it "サフィックス付きリテラル同士でi32とi64を混在させるとエラー" $
      compileSource "fn main() {\n9999i32 + 5i64;\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "boolに対する算術演算はエラー" $
      compileSource "fn main() -> bool {\ntrue + 1\n}"
        `shouldBe` Left "type mismatch: expected bool, found arithmetic expression"
    it "整数変数への否定演算子の適用はエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\n!x;\n}"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "整数リテラルへの否定演算子の適用はエラー" $
      compileSource "fn main() -> bool {\n!5\n}"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "比較結果をintコンテキストで使うとエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nlet y: i32 = 2;\nlet z: i32 = x == y;\nz\n}"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "式文での比較でi32とi64を混在させるとエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\nlet y: i64 = 2;\nx == y;\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "let宣言の右辺での比較でi32とi64を混在させるとエラー" $
      compileSource "fn main() -> bool {\nlet x: i32 = 1;\nlet y: i64 = 2;\nlet z: bool = x == y;\nz\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "大小比較結果をintコンテキストで使うとエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nlet y: i32 = 2;\nlet z: i32 = x < y;\nz\n}"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "式文での大小比較でi32とi64を混在させるとエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\nlet y: i64 = 2;\nx < y;\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "bool同士の大小比較（<）はエラー" $
      compileSource "fn main() -> bool {\nlet x: bool = true;\nlet y: bool = false;\nx < y\n}"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（<=）はエラー" $
      compileSource "fn main() -> bool {\ntrue <= false\n}"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（>）はエラー" $
      compileSource "fn main() -> bool {\ntrue > false\n}"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（>=）はエラー" $
      compileSource "fn main() -> bool {\ntrue >= false\n}"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "i32をas i64で拡大する" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nx as i64\n}"
        `shouldBe` Right ([], TyInt Signed W64, [Push 1, Store W32 (-4), Load W32 (-4), ISext32, Label ".Lfn_end0"])
    it "i64をas i32で縮小する" $
      compileSource "fn main() -> i32 {\nlet x: i64 = 1;\nx as i32\n}"
        `shouldBe` Right ([], TyInt Signed W32, [Push 1, Store W64 (-8), Load W64 (-8), ISext32, Label ".Lfn_end0"])
    it "i64をas i64でキャストすると恒等変換（変換命令なし）" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\nx as i64\n}"
        `shouldBe` Right ([], TyInt Signed W64, [Push 1, Store W64 (-8), Load W64 (-8), Label ".Lfn_end0"])
    it "i32をas i32でキャストするとISext32で再正規化する" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nx as i32\n}"
        `shouldBe` Right ([], TyInt Signed W32, [Push 1, Store W32 (-4), Load W32 (-4), ISext32, Label ".Lfn_end0"])
    it "型なしリテラルのキャストはi64とみなす" $
      compileSource "fn main() -> i32 {\n5000000000 as i32\n}"
        `shouldBe` Right ([], TyInt Signed W32, [Push 5000000000, ISext32, Label ".Lfn_end0"])
    it "型なしリテラルのas i64は変換命令なし" $
      compileSource "fn main() -> i64 {\n1 as i64\n}" `shouldBe` Right ([], TyInt Signed W64, [Push 1, Label ".Lfn_end0"])
    it "boolのキャストはエラー" $
      compileSource "fn main() -> i64 {\nlet x: bool = true;\nx as i64\n}"
        `shouldBe` Left "invalid cast: cannot cast bool to i64"
    it "ポインタのキャストはエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\nlet p: &i64 = &x;\np as i64\n}"
        `shouldBe` Left "invalid cast: cannot cast &i64 to i64"
    it "as i64の結果をi32コンテキストで使うとエラー" $
      compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nlet y: i32 = x as i64;\ny\n}"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "as i32の結果をi64コンテキストで使うとエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\nlet y: i64 = x as i32;\ny\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "as i64の結果をboolコンテキストで使うとエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\nx as i64 == true\n}"
        `shouldSatisfy` isLeft
    it "キャスト無しでの幅違いの代入はエラー（暗黙の型変換は行わない）" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nlet y: i64 = x;\ny\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "u32の加算はIAdd W32の後にIZext32でゼロ拡張する" $
      compileSource "fn main() -> u32 {\n1u32 + 2u32\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 1, Push 2, IAdd W32, IZext32, Label ".Lfn_end0"])
    it "u32変数の読み出しはLoad W32の後にIZext32でゼロ拡張する" $
      compileSource "fn main() -> u32 {\nlet x: u32 = 1;\nx\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 1, Store W32 (-4), Load W32 (-4), IZext32, Label ".Lfn_end0"])
    it "u64の加算はi64と同じ命令列" $
      compileSource "fn main() -> u64 {\n1u64 + 2u64\n}"
        `shouldBe` Right ([], TyInt Unsigned W64, [Push 1, Push 2, IAdd W64, Label ".Lfn_end0"])
    it "u32の除算はIDivU W32" $
      compileSource "fn main() -> u32 {\n7u32 / 2u32\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 7, Push 2, IDivU W32, Label ".Lfn_end0"])
    it "u64の除算はIDivU W64" $
      compileSource "fn main() -> u64 {\n7u64 / 2u64\n}"
        `shouldBe` Right ([], TyInt Unsigned W64, [Push 7, Push 2, IDivU W64, Label ".Lfn_end0"])
    it "u32の大小比較は符号なし比較命令を使う" $
      compileSource "fn main() -> bool {\n1u32 < 2u32 == 3u64 >= 4u64\n}"
        `shouldBe` Right ([], TBool, [Push 1, Push 2, ICmpLtU, Push 3, Push 4, ICmpGeU, ICmpEq, Label ".Lfn_end0"])
    it "u32への型なしリテラルの代入" $
      compileSource "fn main() -> u32 {\nlet x: u32 = 5;\nx\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 5, Store W32 (-4), Load W32 (-4), IZext32, Label ".Lfn_end0"])
    it "&u32のデリファレンスはLoadInd W32の後にIZext32" $
      compileSource "fn main() -> u32 {\nlet a: u32 = 7;\nlet p: &u32 = &a;\n*p\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 7, Store W32 (-4), LoadAddr (-4), Store W64 (-12), Load W64 (-12), LoadInd W32, IZext32, Label ".Lfn_end0"])
    it "i32 as u64は変換元の符号で符号拡張する" $
      compileSource "fn main() -> u64 {\nlet x: i32 = 1;\nx as u64\n}"
        `shouldBe` Right ([], TyInt Unsigned W64, [Push 1, Store W32 (-4), Load W32 (-4), ISext32, Label ".Lfn_end0"])
    it "u32 as i64は変換元の符号でゼロ拡張する" $
      compileSource "fn main() -> i64 {\nlet x: u32 = 1;\nx as i64\n}"
        `shouldBe` Right ([], TyInt Signed W64, [Push 1, Store W32 (-4), Load W32 (-4), IZext32, IZext32, Label ".Lfn_end0"])
    it "i64 as u64は命令なし（ビットの再解釈）" $
      compileSource "fn main() -> u64 {\nlet x: i64 = 1;\nx as u64\n}"
        `shouldBe` Right ([], TyInt Unsigned W64, [Push 1, Store W64 (-8), Load W64 (-8), Label ".Lfn_end0"])
    it "i64 as u32はキャスト先の符号でゼロ拡張する" $
      compileSource "fn main() -> u32 {\nlet x: i64 = 1;\nx as u32\n}"
        `shouldBe` Right ([], TyInt Unsigned W32, [Push 1, Store W64 (-8), Load W64 (-8), IZext32, Label ".Lfn_end0"])
    it "u32 as i32はキャスト先の符号で符号拡張する" $
      compileSource "fn main() -> i32 {\nlet x: u32 = 1;\nx as i32\n}"
        `shouldBe` Right ([], TyInt Signed W32, [Push 1, Store W32 (-4), Load W32 (-4), IZext32, ISext32, Label ".Lfn_end0"])
    it "符号なしと符号ありの混在演算はエラー" $
      compileSource "fn main() {\n1u32 + 1i32;\n}" `shouldBe` Left "type mismatch: u32 and i32"
    it "i32変数をu32へ代入するとエラー" $
      compileSource "fn main() -> u32 {\nlet y: i32 = 1;\nlet x: u32 = y;\nx\n}"
        `shouldBe` Left "type mismatch: expected u32, found i32"
    it "u32とi32の比較はエラー" $
      compileSource "fn main() {\n1u32 < 1i32;\n}" `shouldBe` Left "type mismatch: u32 and i32"
    it "u32とu64の混在演算はエラー" $
      compileSource "fn main() {\n1u32 + 1u64;\n}" `shouldBe` Left "type mismatch: u32 and u64"
    it "符号なし変数への単項マイナスはエラー" $
      compileSource "fn main() -> u32 {\nlet x: u32 = 1;\n-x\n}"
        `shouldBe` Left "cannot apply unary minus to unsigned type u32"
    it "符号なしリテラルへの単項マイナスはエラー" $
      compileSource "fn main() -> u64 {\n-1u64\n}" `shouldBe` Left "cannot apply unary minus to unsigned type u64"
    it "u32の文脈で負の型なしリテラルはエラー" $
      compileSource "fn main() -> u32 {\nlet x: u32 = -1;\nx\n}"
        `shouldBe` Left "cannot apply unary minus to unsigned type u32"
    it "u32の文脈でbool値はエラー" $
      compileSource "fn main() -> u32 {\nlet x: u32 = true;\nx\n}" `shouldBe` Left "type mismatch: expected u32, found bool"
    it "未宣言変数への&はエラー" $
      compileSource "fn main() -> &i64 {\nlet a: &i64 = &b;\na\n}" `shouldBe` Left "undeclared variable: b"
    it "&の結果を誤った型コンテキストで使うとエラー" $
      compileSource "fn main() -> i64 {\nlet a: i64 = 0;\nlet b: i64 = &a;\nb\n}"
        `shouldBe` Left "type mismatch: expected i64, found &i64"
    it "非ポインタのデリファレンスはエラー" $
      compileSource "fn main() -> i64 {\nlet a: i64 = 0;\nlet b: i64 = *a;\nb\n}"
        `shouldBe` Left "type mismatch: expected &i64, found i64"
    it "rvalue（整数リテラル）への&はエラー（lvalueではない）" $
      compileSource "fn main() -> &i64 {\nlet a: &i64 = &5;\na\n}"
        `shouldBe` Left "invalid operand for &: not an lvalue: Lit 5"
    it "rvalue（算術式）への&はエラー" $
      compileSource "fn main() -> &i64 {\nlet a: i64 = 1;\nlet b: i64 = 2;\nlet c: &i64 = &(a + b);\nc\n}"
        `shouldBe` Left "invalid operand for &: not an lvalue: Add (Var \"a\") (Var \"b\")"
    it "ポインタの加算はエラー（ポインタ演算は未対応）" $
      compileSource "fn main() -> &i64 {\nlet a: i64 = 0;\nlet b: &i64 = &a;\nlet c: &i64 = b + 1;\nc\n}"
        `shouldBe` Left "type mismatch: expected &i64, found arithmetic expression"
    it "&*pはpと同じ型・同じ値になる" $ do
      compileSource "fn main() -> i64 {\nlet a: i64 = 9;\nlet p: &i64 = &a;\nlet q: &i64 = &*p;\n*q\n}" `shouldSatisfy` isRight
    it "ポインタ同士の等価比較は許可される" $
      compileSource "fn main() -> bool {\nlet a: i64 = 0;\nlet b: &i64 = &a;\nlet c: &i64 = &a;\nb == c\n}" `shouldSatisfy` isRight
    it "ブロックを抜けた後の内部宣言変数の参照はエラー" $
      compileSource "fn main() {\n{\nlet x: i64 = 1;\n}\nx\n}" `shouldBe` Left "undeclared variable: x"
    it "同一ブロック内での同名再宣言はエラー" $
      compileSource "fn main() -> i64 {\n{\nlet x: i64 = 1;\nlet x: i64 = 2;\n}\n1\n}"
        `shouldBe` Left "variable already declared: x"
    it "外側と同名の変数をブロック内でletしてもエラーにならない（シャドーイング）" $
      compileSource "fn main() -> i64 {\nlet x: i64 = 1;\n{\nlet x: i64 = 2;\n}\nx\n}" `shouldSatisfy` isRight
    it "let宣言（型注釈省略）: 無型リテラルはi64にデフォルトされる" $
      compileSource "fn main() -> i64 {\nlet x = 5;\nx\n}"
        `shouldBe` Right ([], TyInt Signed W64, [Push 5, Store W64 (-8), Load W64 (-8), Label ".Lfn_end0"])
    it "let宣言（型注釈省略）: サフィックス付きリテラルはその型で確定する" $
      compileSource "fn main() -> i32 {\nlet x = 5i32;\nx\n}"
        `shouldBe` Right ([], TyInt Signed W32, [Push 5, Store W32 (-4), Load W32 (-4), Label ".Lfn_end0"])
    it "let宣言（型注釈省略）: boolリテラルはboolとして推論される" $
      compileSource "fn main() -> bool {\nlet x = true;\nx\n}"
        `shouldBe` Right ([], TBool, [Push 1, Store W32 (-4), Load W32 (-4), Label ".Lfn_end0"])
    it "let宣言（型注釈省略）: &lvalueからポインタ型が推論される" $
      compileSource "fn main() -> i32 {\nlet a: i32 = 1;\nlet p = &a;\n*p\n}"
        `shouldBe` Right
          ( []
          , TyInt Signed W32
          , [Push 1, Store W32 (-4), LoadAddr (-4), Store W64 (-12), Load W64 (-12), LoadInd W32, Label ".Lfn_end0"]
          )
    it "let宣言（型注釈省略）: 関数呼び出しの戻り値型が推論される" $
      compileSource "fn f() -> i32 {\n1\n}\nfn main() -> i32 {\nlet x = f();\nx\n}"
        `shouldBe` Right
          ( [("f", [Push 1, Label ".Lfn_end0"])]
          , TyInt Signed W32
          , [ICall "f" 0, Store W32 (-4), Load W32 (-4), Label ".Lfn_end1"]
          )
    it "let宣言（型注釈省略）: 無型リテラルとboolの算術演算は型不一致エラー（inferMaybeTypeの単一化はi32/i64のみ検出しないが、後続のcompileExprTypedで検出される）" $
      compileSource "fn main() -> bool {\nlet x = 5 + true;\nx\n}"
        `shouldBe` Left "type mismatch: expected bool, found arithmetic expression"
    it "let宣言（型注釈省略）: 推論結果と宣言済み変数のi32/i64混在はエラー" $
      compileSource "fn main() {\nlet x: i32 = 1;\nlet y = x + 1i64;\ny\n}"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "let宣言（型注釈省略）と同名の再宣言（同一ブロック内）はエラー" $
      compileSource "fn main() -> i64 {\nlet x = 1;\nlet x = 2;\nx\n}"
        `shouldBe` Left "variable already declared: x"
    it "if文の条件式が整数リテラルはエラー" $
      compileSource "fn main() -> i64 {\nif 1 {\n}\n1\n}" `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "if文の条件式が整数変数はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nif x {\n}\n1\n}"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "if文の本体を抜けた後の内部宣言変数の参照はエラー" $
      compileSource "fn main() {\nif true {\nlet x: i64 = 1;\n}\nx\n}" `shouldBe` Left "undeclared variable: x"
    it "else if文の条件式が非bool式はエラー" $
      compileSource "fn main() -> i64 {\nif false {\n} else if 1 {\n}\n1\n}"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "while文の条件式が整数リテラルはエラー" $
      compileSource "fn main() -> i64 {\nwhile 1 {\n}\n1\n}"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "while文の条件式が整数変数はエラー" $
      compileSource "fn main() -> i64 {\nlet x: i32 = 1;\nwhile x {\n}\n1\n}"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "while文の本体を抜けた後の内部宣言変数の参照はエラー" $
      compileSource "fn main() {\nwhile true {\nlet x: i64 = 1;\nbreak;\n}\nx\n}" `shouldBe` Left "undeclared variable: x"
    it "break文をループ外で使うとエラー" $
      compileSource "fn main() -> i64 {\nbreak;\n1\n}" `shouldBe` Left "break used outside loop"
    it "continue文をループ外で使うとエラー" $
      compileSource "fn main() -> i64 {\ncontinue;\n1\n}" `shouldBe` Left "continue used outside loop"
    it "break文をif文の中（ループ外）で使うとエラー" $
      compileSource "fn main() -> i64 {\nif true {\nbreak;\n}\n1\n}" `shouldBe` Left "break used outside loop"
    it "break文をwhileの外側のブロックで使うとエラー（ループを抜けた後は無効）" $
      compileSource "fn main() -> i64 {\nwhile true {\nbreak;\n}\n{\nbreak;\n}\n1\n}" `shouldBe` Left "break used outside loop"

  describe "意味論エラー（fn定義・呼び出し）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "未定義関数の呼び出しはエラー" $
      compileSource "fn main() {\nf()\n}" `shouldBe` Left "undeclared function: f"
    it "引数個数が足りない呼び出しはエラー" $
      compileSource "fn f(a: i64, b: i64) -> i64 {\na + b\n}\nfn main() -> i64 {\nf(1)\n}"
        `shouldBe` Left "wrong number of arguments for f: expected 2, found 1"
    it "引数個数が多すぎる呼び出しはエラー" $
      compileSource "fn f(a: i64) -> i64 {\na\n}\nfn main() -> i64 {\nf(1, 2)\n}"
        `shouldBe` Left "wrong number of arguments for f: expected 1, found 2"
    it "引数の型が宣言と異なる呼び出しはエラー（暗黙変換は行わない）" $
      compileSource "fn f(a: i64) -> i64 {\na\n}\nfn main() -> i64 {\nlet x: i32 = 1;\nf(x)\n}"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "戻り値の型が期待と異なるとエラー" $
      compileSource "fn f() -> i64 {\n1\n}\nfn main() -> i32 {\nlet x: i32 = f();\nx\n}"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "同名の関数を再定義するとエラー" $
      compileSource "fn f() -> i64 {\n1\n}\nfn f() -> i64 {\n2\n}\nfn main() {\n3\n}"
        `shouldBe` Left "function already declared: f"
    it "引数が7個を超える関数定義はエラー（レジスタ渡しの上限）" $
      compileSource
        "fn f(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, g: i64) -> i64 {\na\n}\nfn main() {\nf(1,2,3,4,5,6,7)\n}"
        `shouldBe` Left "too many parameters (max 6): f"
    it "宣言順に依存しない相互再帰は成功する（二パスコンパイルの確認）" $
      compileSource
        "fn is_even(n: i64) -> bool {\nif n == 0 {\nreturn true;\n}\nis_odd(n - 1)\n}\nfn is_odd(n: i64) -> bool {\nif n == 0 {\nreturn false;\n}\nis_even(n - 1)\n}\nfn main() -> bool {\nis_even(4)\n}"
        `shouldSatisfy` isRight
    it "仮引数の型注釈が無いfnはエラー（推論はしない）" $
      compileSource "fn id(x) -> i64 {\nx\n}\nfn main() -> i64 {\nid(5)\n}"
        `shouldBe` Left "expected ':' and type after parameter name: x"
    it "戻り値型を省略したfnの呼び出しは () 型（値を期待する位置ではエラー）" $
      compileSource "fn f() {}\nfn main() -> i64 {\nf()\n}" `shouldSatisfy` isLeft

  describe "run（VM）" $ do
    it "Load/Storeで変数の値を保持する" $
      run [Push 1, Store W64 (-8), Load W64 (-8), Push 2, IAdd W64] `shouldBe` Right 3
    it "IAdd W32はi32範囲でラップアラウンドする" $
      run [Push 2147483647, Push 1, IAdd W32] `shouldBe` Right (-2147483648)
    it "IDiv W32で最小値を-1で割るとi32範囲でラップアラウンドする" $
      run [Push (-2147483648), Push (-1), IDiv W32] `shouldBe` Right (-2147483648)
    it "IDiv W64で最小値を-1で割るとi64範囲でラップアラウンドする" $
      run [Push minBound, Push (-1), IDiv W64] `shouldBe` Right minBound
    it "IZext32は下位32bitをゼロ拡張する" $
      run [Push (-1), IZext32] `shouldBe` Right 4294967295
    it "IDivU W32は符号なしで除算する" $
      run [Push 4294967295, Push 2, IDivU W32] `shouldBe` Right 2147483647
    it "IDivU W64は符号なしで除算する（-1をu64の最大値として扱う）" $
      run [Push (-1), Push 2, IDivU W64] `shouldBe` Right maxBound
    it "IDivUもゼロ除算はエラー" $
      run [Push 1, Push 0, IDivU W64] `shouldBe` Left "division by zero"
    it "ICmpLtUは-1をu64の最大値として比較する" $
      run [Push 1, Push (-1), ICmpLtU] `shouldBe` Right 1
    it "ICmpGtUは-1をu64の最大値として比較する" $
      run [Push (-1), Push 1, ICmpGtU] `shouldBe` Right 1
    it "Store W32はi32範囲に切り詰める" $
      run [Push 4294967296, Store W32 (-4), Load W32 (-4)] `shouldBe` Right 0
    it "ICmpEqは等しい値で1を返す" $
      run [Push 3, Push 3, ICmpEq] `shouldBe` Right 1
    it "ICmpEqは異なる値で0を返す" $
      run [Push 3, Push 4, ICmpEq] `shouldBe` Right 0
    it "ICmpNeは異なる値で1を返す" $
      run [Push 3, Push 4, ICmpNe] `shouldBe` Right 1
    it "ICmpLtはa<bで1を返す" $
      run [Push 3, Push 4, ICmpLt] `shouldBe` Right 1
    it "ICmpLtはa>=bで0を返す" $
      run [Push 4, Push 3, ICmpLt] `shouldBe` Right 0
    it "ICmpLeはa==bで1を返す" $
      run [Push 3, Push 3, ICmpLe] `shouldBe` Right 1
    it "ICmpLeはa>bで0を返す" $
      run [Push 4, Push 3, ICmpLe] `shouldBe` Right 0
    it "ICmpGtはa>bで1を返す" $
      run [Push 4, Push 3, ICmpGt] `shouldBe` Right 1
    it "ICmpGtはa<=bで0を返す" $
      run [Push 3, Push 4, ICmpGt] `shouldBe` Right 0
    it "ICmpGeはa==bで1を返す" $
      run [Push 3, Push 3, ICmpGe] `shouldBe` Right 1
    it "ICmpGeはa<bで0を返す" $
      run [Push 3, Push 4, ICmpGe] `shouldBe` Right 0
    it "INotは0を1に反転する" $
      run [Push 0, INot] `shouldBe` Right 1
    it "INotは1を0に反転する" $
      run [Push 1, INot] `shouldBe` Right 0
    it "ISext32はi32範囲内の値をそのまま保持する（as i64での値保存の確認）" $
      run [Push 42, ISext32] `shouldBe` Right 42
    it "ISext32はi32範囲外の値をラップアラウンドさせる（as i32での縮小変換の確認）" $
      run [Push 4294967296, ISext32] `shouldBe` Right 0
    it "ISext32はi32範囲外の値を符号拡張してラップアラウンドさせる" $
      run [Push 2147483648, ISext32] `shouldBe` Right (-2147483648)
    it "LoadAddr/LoadIndの往復で変数の値を読み出せる" $
      run [Push 5, Store W64 (-8), LoadAddr (-8), LoadInd W64] `shouldBe` Right 5

  describe "compile + codegen + gcc（結合テスト）" $ do
    it "リテラルを評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Lit 7)
      result `shouldBe` "7"
    it "加算を評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Add (Lit 3) (Lit 4))
      result `shouldBe` "7"
    it "減算を評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Sub (Lit 5) (Lit 3))
      result `shouldBe` "2"
    it "乗算を評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Mul (Lit 2) (Lit 6))
      result `shouldBe` "12"
    it "除算を評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Div (Lit 6) (Lit 3))
      result `shouldBe` "2"
    it "単項マイナスを評価する" $ do
      result <- compileAndRun (TyInt Signed W64) (Neg (Lit 5))
      result `shouldBe` "-5"
    it "複合式を評価する: 1 + 2 * (3 - 4) = -1" $ do
      result <- compileAndRun (TyInt Signed W64) (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4))))
      result `shouldBe` "-1"
    it "boolリテラル（true）を評価する" $ do
      result <- compileAndRun TBool (BoolLit True)
      result `shouldBe` "true"
    it "boolリテラル（false）を評価する" $ do
      result <- compileAndRun TBool (BoolLit False)
      result `shouldBe` "false"
    it "等価演算子を評価する（等しい場合）" $ do
      result <- compileAndRun TBool (Eq (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "等価演算子を評価する（等しくない場合）" $ do
      result <- compileAndRun TBool (Eq (Lit 3) (Lit 4))
      result `shouldBe` "false"
    it "非等価演算子を評価する" $ do
      result <- compileAndRun TBool (Neq (Lit 3) (Lit 4))
      result `shouldBe` "true"
    it "小なり演算子を評価する（true）" $ do
      result <- compileAndRun TBool (Lt (Lit 3) (Lit 4))
      result `shouldBe` "true"
    it "小なり演算子を評価する（false）" $ do
      result <- compileAndRun TBool (Lt (Lit 4) (Lit 3))
      result `shouldBe` "false"
    it "以下演算子を評価する（等しい場合はtrue）" $ do
      result <- compileAndRun TBool (Le (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "大なり演算子を評価する（true）" $ do
      result <- compileAndRun TBool (Gt (Lit 4) (Lit 3))
      result `shouldBe` "true"
    it "大なり演算子を評価する（false）" $ do
      result <- compileAndRun TBool (Gt (Lit 3) (Lit 4))
      result `shouldBe` "false"
    it "以上演算子を評価する（等しい場合はtrue）" $ do
      result <- compileAndRun TBool (Ge (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "否定演算子を評価する" $ do
      result <- compileAndRun TBool (Not (BoolLit False))
      result `shouldBe` "true"
    it "リテラル as i32を評価する" $ do
      result <- compileAndRun (TyInt Signed W32) (Cast (Lit 64) (TyInt Signed W32))
      result `shouldBe` "64"

  describe "compile + codegen + gcc（変数を含むソースの結合テスト）" $ do
    it "複数の宣言・代入・末尾式を評価する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\nlet y: i64 = 2;\nx = x + y;\nx\n}"
      result `shouldBe` "3"
    it "変数1個（奇数オフセット）でもアライメントが崩れない" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 5;\nx + 1\n}"
      result `shouldBe` "6"
    it "i32変数の宣言・演算・出力を評価する" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 1;\nlet y: i32 = 2;\nx = x + y;\nx\n}"
      result `shouldBe` "3"
    it "i32のオーバーフローは32bit範囲でラップアラウンドする" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 2147483647;\nx + 1\n}"
      result `shouldBe` "-2147483648"
    it "i32の境界値（INT32_MIN）を正しく扱う（movabsq修正の確認）" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = -2147483648;\nx\n}"
      result `shouldBe` "-2147483648"
    it "i32の最小値を-1で割るとラップアラウンドする（idivlの例外を起こさない）" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = -2147483648;\nlet m: i32 = -1;\nx / m\n}"
      result `shouldBe` "-2147483648"
    it "i64の最小値を-1で割るとラップアラウンドする（idivqの例外を起こさない）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = -9223372036854775808;\nlet m: i64 = -1;\nx / m\n}"
      result `shouldBe` "-9223372036854775808"
    it "-1での割り算は符号反転と同じ結果になる" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 7;\nlet m: i32 = -1;\nx / m\n}"
      result `shouldBe` "-7"
    it "i32とi64が交互に宣言されてもタイトパッキングで正しく動作する（式自体は単一型を維持）" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet a: i32 = 1;\nlet b: i64 = 2;\nlet c: i32 = 3;\na + c\n}"
      result `shouldBe` "4"
    it "i64の末尾式は%ldで正しく出力される（64bit値の潜在バグ修正の確認）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\nlet y: i64 = 2;\ny\n}"
      result `shouldBe` "2"
    it "i64サフィックス付きリテラルはlet無しでもi64として%ldで出力される" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\n9999i64\n}"
      result `shouldBe` "9999"
    it "i32サフィックス付きリテラルはlet無しでもi32として%dで出力される" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\n9999i32\n}"
      result `shouldBe` "9999"
    it "i64サフィックス付きリテラルをi64変数に束縛して評価する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 9999i64;\nx + 1\n}"
      result `shouldBe` "10000"
    it "i32サフィックス付きリテラルをi32変数に束縛して評価する" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 9999i32;\nx + 1\n}"
      result `shouldBe` "10000"
    it "サフィックス付きリテラルと宣言型が食い違うとコンパイルエラーになる" $
      (tokenize "fn main() -> i64 {\nlet x: i64 = 9999i32;\nx\n}" >>= parse >>= compile)
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "let宣言（型注釈省略）: 無型リテラルはi64にデフォルトされ%ldで出力される" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x = 5;\nx + 1\n}"
      result `shouldBe` "6"
    it "let宣言（型注釈省略）: サフィックス付きリテラルからi32が推論され%dで出力される" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x = 9999i32;\nx + 1\n}"
      result `shouldBe` "10000"
    it "let宣言（型注釈省略）と型注釈ありのletが混在しても正しく動作する" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 1;\nlet y = x + 2;\ny\n}"
      result `shouldBe` "3"
    it "let宣言（型注釈省略）: &lvalueからポインタ型を推論し、デリファレンスで元の値を復元する" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet a: i32 = 42;\nlet p = &a;\n*p\n}"
      result `shouldBe` "42"
    it "bool変数の宣言・末尾式出力を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet flag: bool = true;\nflag\n}"
      result `shouldBe` "true"
    it "bool変数への代入を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet flag: bool = true;\nflag = false;\nflag\n}"
      result `shouldBe` "false"
    it "i32変数同士の等価比較を評価する（true）" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: i32 = 3;\nlet b: i32 = 3;\na == b\n}"
      result `shouldBe` "true"
    it "i32変数同士の等価比較を評価する（false）" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: i32 = 3;\nlet b: i32 = 4;\na == b\n}"
      result `shouldBe` "false"
    it "i32変数同士の非等価比較を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: i32 = 3;\nlet b: i32 = 4;\na != b\n}"
      result `shouldBe` "true"
    it "i64変数同士の等価比較を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet x: i64 = 100;\nlet y: i64 = 100;\nx == y\n}"
      result `shouldBe` "true"
    it "否定演算子と括弧を組み合わせて評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\n!(1 == 2)\n}"
      result `shouldBe` "true"
    it "算術式の結果同士の比較を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet x: i32 = 1;\nlet y: i32 = 2;\n(x + 1) == y\n}"
      result `shouldBe` "true"
    it "i32変数同士の小なり比較を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: i32 = 3;\nlet b: i32 = 4;\na < b\n}"
      result `shouldBe` "true"
    it "i32変数同士の以下比較を評価する（等しい場合）" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: i32 = 3;\nlet b: i32 = 3;\na <= b\n}"
      result `shouldBe` "true"
    it "i64変数同士の大なり比較を評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet x: i64 = 100;\nlet y: i64 = 50;\nx > y\n}"
      result `shouldBe` "true"
    it "i64変数同士の以上比較を評価する（等しい場合）" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet x: i64 = 100;\nlet y: i64 = 100;\nx >= y\n}"
      result `shouldBe` "true"
    it "否定演算子と小なり比較を組み合わせて評価する" $ do
      result <- compileSourceAndRun "fn main() -> bool {\n!(1 < 2)\n}"
      result `shouldBe` "false"
    it "as i64はi32の値をそのままi64として保持する（拡大変換は値を保存する）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i32 = 2147483647;\nlet y: i64 = x as i64;\ny\n}"
      result `shouldBe` "2147483647"
    it "as i64は負のi32値の符号を保存する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i32 = -5;\nlet y: i64 = x as i64;\ny\n}"
      result `shouldBe` "-5"
    it "as i32はi32範囲外のi64値をラップアラウンドさせる（縮小変換）" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i64 = 4294967296;\nlet y: i32 = x as i32 + 5;\ny\n}"
      result `shouldBe` "5"
    it "as i32で縮小すると上位ビットが切り捨てられ符号拡張される" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i64 = 2147483648;\nx as i32\n}"
      result `shouldBe` "-2147483648"
    it "as i32とas i64のラウンドトリップ（i32->i64->i32）は値を保存する" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 42;\nx as i64 as i32\n}"
      result `shouldBe` "42"
    it "被演算子をas i64で拡大すればi32の範囲を超える演算ができる" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet a: i32 = 2147483647;\nlet b: i64 = 1;\na as i64 + b\n}"
      result `shouldBe` "2147483648"
    it "to_i64は識別子として使える" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet to_i64 = 1;\nto_i64\n}"
      result `shouldBe` "1"
    it "u32の減算でアンダーフローするとラップアラウンドする" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet a: u32 = 1;\nlet b: u32 = 2;\na - b\n}"
      result `shouldBe` "4294967295"
    it "u64の減算でアンダーフローするとラップアラウンドする" $ do
      result <- compileSourceAndRun "fn main() -> u64 {\n0u64 - 1u64\n}"
      result `shouldBe` "18446744073709551615"
    it "u32の乗算でオーバーフローするとラップアラウンドする" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\n4294967295u32 * 2u32\n}"
      result `shouldBe` "4294967294"
    it "u32の加算でオーバーフローすると0に戻る" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet a: u32 = 4294967295;\na + 1u32\n}"
      result `shouldBe` "0"
    it "u32の除算は符号なしで行う" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet a: u32 = 4294967295;\na / 2u32\n}"
      result `shouldBe` "2147483647"
    it "u64の除算は符号なしで行う" $ do
      result <- compileSourceAndRun "fn main() -> u64 {\n18446744073709551615u64 / 2u64\n}"
      result `shouldBe` "9223372036854775807"
    it "u32の大小比較は符号なしで行う" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet a: u32 = 4294967295;\na > 1u32\n}"
      result `shouldBe` "true"
    it "u64の大小比較は符号なしで行う" $ do
      result <- compileSourceAndRun "fn main() -> bool {\n18446744073709551615u64 > 1u64\n}"
      result `shouldBe` "true"
    it "-1i32 as u32は4294967295（同幅のビット再解釈）" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet x: i32 = -1;\nx as u32\n}"
      result `shouldBe` "4294967295"
    it "-1i32 as u64は符号拡張してから再解釈する" $ do
      result <- compileSourceAndRun "fn main() -> u64 {\nlet x: i32 = -1;\nx as u64\n}"
      result `shouldBe` "18446744073709551615"
    it "4294967295u32 as i32は-1" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: u32 = 4294967295;\nx as i32\n}"
      result `shouldBe` "-1"
    it "4294967295u32 as i64はゼロ拡張して値を保存する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: u32 = 4294967295;\nx as i64\n}"
      result `shouldBe` "4294967295"
    it "4294967296u64 as u32は下位32bitに切り詰める" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet x: u64 = 4294967296;\nx as u32\n}"
      result `shouldBe` "0"
    it "u64 as i64はビットパターンを再解釈する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\n18446744073709551615u64 as i64\n}"
      result `shouldBe` "-1"
    it "u32の引数・戻り値を持つ関数を評価する" $ do
      result <- compileSourceAndRun "fn dec(x: u32) -> u32 {\nx - 1u32\n}\nfn main() -> u32 {\ndec(0u32)\n}"
      result `shouldBe` "4294967295"
    it "&u32経由でu32の値を読み出す" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet a: u32 = 4294967295;\nlet p: &u32 = &a;\n*p / 2u32\n}"
      result `shouldBe` "2147483647"
    it "ブロック内からの代入は外側の変数へ反映される" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\n{\nx = 2;\n}\nx\n}"
      result `shouldBe` "2"
    it "シャドーイングされた変数はブロックを抜けると外側の値に戻る" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\n{\nlet x: i64 = 99;\n}\nx\n}"
      result `shouldBe` "1"
    it "兄弟ブロックはそれぞれ独立にローカル変数を持てる（同名変数の再利用を含む）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet sum: i64 = 0;\n{\nlet y: i64 = 1;\nsum = sum + y;\n}\n{\nlet y: i64 = 2;\nsum = sum + y;\n}\nsum\n}"
      result `shouldBe` "3"
    it "空ブロックは合法で何もしない" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\n{}\nx\n}"
      result `shouldBe` "1"

  describe "compile + codegen + gcc（if文の結合テスト）" $ do
    it "条件が真ならif本体を実行する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\nif true {\nx = 1;\n}\nx\n}"
      result `shouldBe` "1"
    it "条件が偽ならif本体を実行しない（elseなし）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\nif false {\nx = 1;\n}\nx\n}"
      result `shouldBe` "0"
    it "条件が偽ならelse本体を実行する" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\nif false {\nx = 1;\n} else {\nx = 2;\n}\nx\n}"
      result `shouldBe` "2"
    it "条件が真ならelseは実行されない" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\nif true {\nx = 1;\n} else {\nx = 2;\n}\nx\n}"
      result `shouldBe` "1"
    it "比較演算子を条件式に使える" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 3;\nlet y: i32 = 0;\nif x == 3 {\ny = 1;\n} else {\ny = 2;\n}\ny\n}"
      result `shouldBe` "1"
    it "大小比較演算子を条件式に使える" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 3;\nlet y: i32 = 0;\nif x < 5 {\ny = 1;\n} else {\ny = 2;\n}\ny\n}"
      result `shouldBe` "1"
    it "else ifで最初に真になった分岐だけを実行する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 2;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n} else if x == 2 {\ny = 3;\n} else {\ny = 4;\n}\ny\n}"
      result `shouldBe` "2"
    it "else ifが全て偽ならelse本体を実行する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 9;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n} else {\ny = 3;\n}\ny\n}"
      result `shouldBe` "3"
    it "else ifが全て偽でelseも無ければ何も実行しない" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 9;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n}\ny\n}"
      result `shouldBe` "0"
    it "ifをネストできる" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 1;\nif x == 1 {\nif x == 1 {\nx = 42;\n}\n}\nx\n}"
      result `shouldBe` "42"
    it "ブロック内にifをネストできる" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 1;\n{\nif x == 1 {\nx = 42;\n}\n}\nx\n}"
      result `shouldBe` "42"
    it "then節とelse節はそれぞれ独立したローカル変数を持てる（同名変数の再利用を含む）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet sum: i64 = 0;\nif true {\nlet y: i64 = 1;\nsum = sum + y;\n} else {\nlet y: i64 = 99;\nsum = sum + y;\n}\nsum\n}"
      result `shouldBe` "1"
    it "if本体を抜けるとその中で宣言した変数は不可視になる（シャドーイングも含めた回帰確認）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 1;\nif true {\nlet x: i64 = 99;\nx = 2;\n}\nx\n}"
      result `shouldBe` "1"
    it "複数のifが連続しても互いに独立して動作する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet a: i64 = 0;\nlet b: i64 = 0;\nif true {\na = 1;\n} else {\na = 2;\n}\nif false {\nb = 1;\n} else {\nb = 2;\n}\na + b\n}"
      result `shouldBe` "3"

  describe "compile + codegen + gcc（while文の結合テスト）" $ do
    it "基本的なカウントダウンループを実行する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 3;\nlet sum: i64 = 0;\nwhile x != 0 {\nsum = sum + x;\nx = x - 1;\n}\nsum\n}"
      result `shouldBe` "6"
    it "条件が最初から偽なら本体を一度も実行しない" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\nwhile x != 0 {\nx = 1;\n}\nx\n}"
      result `shouldBe` "0"
    it "breakでループを早期終了する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 0;\nwhile true {\nx = x + 1;\nif x == 3 {\nbreak;\n}\n}\nx\n}"
      result `shouldBe` "3"
    it "continueで本体の残りをスキップし条件を再評価する" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 0;\nlet sum: i64 = 0;\nwhile x != 5 {\nx = x + 1;\nif x == 3 {\ncontinue;\n}\nsum = sum + x;\n}\nsum\n}"
      result `shouldBe` "12"
    it "whileをネストできる（内側のbreakは外側に影響しない）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet i: i64 = 0;\nlet count: i64 = 0;\nwhile i != 3 {\nlet j: i64 = 0;\nwhile j != 5 {\nif j == 2 {\nbreak;\n}\ncount = count + 1;\nj = j + 1;\n}\ni = i + 1;\n}\ncount\n}"
      result `shouldBe` "6"
    it "whileのネストで内側のcontinueは外側ループに影響しない" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet i: i64 = 0;\nlet count: i64 = 0;\nwhile i != 2 {\nlet j: i64 = 0;\nwhile j != 3 {\nj = j + 1;\nif j == 2 {\ncontinue;\n}\ncount = count + 1;\n}\ni = i + 1;\n}\ncount\n}"
      result `shouldBe` "4"
    it "while本体を抜けるとその中で宣言した変数は不可視になる（毎イテレーション巻き戻る）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet x: i64 = 0;\nlet i: i64 = 0;\nwhile i != 3 {\nlet y: i64 = 100;\ny = y + 1;\ni = i + 1;\n}\nx\n}"
      result `shouldBe` "0"
    it "ブロック内にwhileをネストできる" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet x: i64 = 0;\n{\nwhile x != 3 {\nx = x + 1;\n}\n}\nx\n}"
      result `shouldBe` "3"
    it "複数のwhileが連続しても互いに独立して動作する（ラベルの一意性の間接的な確認）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet a: i64 = 0;\nwhile a != 2 {\na = a + 1;\n}\nlet b: i64 = 0;\nwhile b != 3 {\nb = b + 1;\n}\na + b\n}"
      result `shouldBe` "5"
    it "小なり演算子をwhileの条件式に使える（sample/src008.sl相当）" $ do
      result <- compileSourceAndRun
        "fn main() -> i64 {\nlet a: i64 = 0;\nlet sum: i64 = 0;\nwhile a < 10 {\nsum = sum + a;\na = a + 1;\n}\nsum\n}"
      result `shouldBe` "45"

  describe "compile + codegen + gcc（fn定義・呼び出しの結合テスト）" $ do
    it "単純な関数呼び出しを評価する" $ do
      result <- compileSourceAndRun "fn add(a: i64, b: i64) -> i64 {\na + b\n}\nfn main() -> i64 {\nlet x: i64 = add(1, 2);\nx\n}"
      result `shouldBe` "3"
    it "引数の型がi32の関数を評価する" $ do
      result <- compileSourceAndRun "fn add(a: i32, b: i32) -> i32 {\na + b\n}\nfn main() -> i32 {\nadd(1, 2)\n}"
      result `shouldBe` "3"
    it "戻り値がboolの関数を評価する" $ do
      result <- compileSourceAndRun "fn isPositive(x: i64) -> bool {\nx > 0\n}\nfn main() -> bool {\nisPositive(5)\n}"
      result `shouldBe` "true"
    it "早期returnを評価する" $ do
      result <- compileSourceAndRun
        "fn abs(x: i64) -> i64 {\nif x < 0 {\nreturn 0 - x;\n}\nx\n}\nfn main() -> i64 {\nabs(0 - 5)\n}"
      result `shouldBe` "5"
    it "自己再帰（階乗）を評価する" $ do
      result <- compileSourceAndRun
        "fn fact(n: i64) -> i64 {\nif n <= 1 {\nreturn 1;\n}\nn * fact(n - 1)\n}\nfn main() -> i64 {\nfact(10)\n}"
      result `shouldBe` "3628800"
    it "宣言順に依存しない相互再帰を評価する" $ do
      result <- compileSourceAndRun
        "fn is_even(n: i64) -> bool {\nif n == 0 {\nreturn true;\n}\nis_odd(n - 1)\n}\nfn is_odd(n: i64) -> bool {\nif n == 0 {\nreturn false;\n}\nis_even(n - 1)\n}\nfn main() -> bool {\nis_even(10)\n}"
      result `shouldBe` "true"
    it "6個の引数（レジスタ渡しの上限）を持つ関数を評価する" $ do
      result <- compileSourceAndRun
        "fn sum6(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64) -> i64 {\na + b + c + d + e + f\n}\nfn main() -> i64 {\nsum6(1, 2, 3, 4, 5, 6)\n}"
      result `shouldBe` "21"
    it "呼び出しは他の呼び出しの引数として入れ子にできる" $ do
      result <- compileSourceAndRun
        "fn inc(x: i64) -> i64 {\nx + 1\n}\nfn main() -> i64 {\ninc(inc(inc(0)))\n}"
      result `shouldBe` "3"
    it "式の途中（奇数深さ）でのユーザー関数呼び出しでもスタックアライメントが崩れない" $ do
      result <- compileSourceAndRun "fn foo(x: i64) -> i64 {\nx * 2\n}\nfn main() -> i64 {\n1 + 2 + foo(3)\n}"
      result `shouldBe` "9"
    it "mainより後に定義した関数を呼び出せる（実行結果の確認）" $ do
      result <- compileSourceAndRun
        "fn double(x: i64) -> i64 {\nx * 2\n}\nfn main() -> i64 {\nlet a: i64 = 1;\nlet b: i64 = double(a);\ntriple(b)\n}\nfn triple(x: i64) -> i64 {\nx * 3\n}"
      result `shouldBe` "6"
  describe "compile + codegen + gcc（ポインタ型の結合テスト）" $ do
    it "&で取得したアドレスを*で読み戻す（仕様例）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet a:i64 = 0;\nlet b:&i64 = &a;\nlet c:i64 = *b;\nc\n}"
      result `shouldBe` "0"
    it "&aで取得したアドレスは実体を指す（aへの再代入後の*bが変更後の値を返す）" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet a:i64 = 1;\nlet b:&i64 = &a;\na = 42;\nlet c:i64 = *b;\nc\n}"
      result `shouldBe` "42"
    it "&&i64（ポインタのポインタ）の往復" $ do
      result <-
        compileSourceAndRun
          "fn main() -> i64 {\nlet a:i64 = 7;\nlet b:&i64 = &a;\nlet c:&&i64 = &b;\nlet d:i64 = **c;\nd\n}"
      result `shouldBe` "7"
    it "&*p はpと同じ値を指す" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet a:i64 = 9;\nlet p:&i64 = &a;\nlet q:&i64 = &*p;\n*q\n}"
      result `shouldBe` "9"
    it "ポインタ型の関数引数・戻り値を扱える" $ do
      result <-
        compileSourceAndRun
          "fn deref(p: &i64) -> i64 {\n*p\n}\nfn main() -> i64 {\nlet a:i64 = 3;\nderef(&a)\n}"
      result `shouldBe` "3"
    it "末尾式がポインタ型のとき%p形式（0xで始まる）で出力する" $ do
      result <- compileSourceAndRun "fn main() -> &i64 {\nlet a:i64 = 0;\nlet b:&i64 = &a;\nb\n}"
      take 2 result `shouldBe` "0x"

  describe "ゼロ除算の実行時エラー" $ do
    it "変数なしのゼロ除算はエラーメッセージを出力して非ゼロ終了する" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> i64 {\n1 / 0\n}"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "変数がある状態でのゼロ除算でもアライメントが崩れない" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> i64 {\nlet x: i64 = 5;\nx / 0\n}"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "i32変数のゼロ除算もエラーメッセージを出力して非ゼロ終了する" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> i32 {\nlet x: i32 = 5;\nx / 0\n}"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "u32変数のゼロ除算もエラーメッセージを出力して非ゼロ終了する（IDivU W32）" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> u32 {\nlet x: u32 = 5;\nx / 0\n}"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "u64のゼロ除算もエラーメッセージを出力して非ゼロ終了する（IDivU W64）" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> u64 {\nlet x: u64 = 5;\nx / 0\n}"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess

  describe "配列・usize（tokenize/parse）" $ do
    it "[ ] を変換する" $
      tokenize "a[0]" `shouldBe` Right [TIdent "a", TLBracket, TInt 0, TRBracket]
    it "usizeサフィックス付き整数リテラルを変換する" $
      tokenize "5usize" `shouldBe` Right [TIntSuffixed 5 TUsize]
    it "usizeの後に識別子文字が続く場合はサフィックスとみなさない" $
      tokenize "5usizex" `shouldBe` Right [TInt 5, TIdent "usizex"]
    let parseSource src = tokenize src >>= fmap splitMain . parse
    it "配列型の型注釈と配列リテラル" $
      parseSource "fn main() -> i64 {\nlet a: [i32; 3] = [1, 2, 3];\n0\n}"
        `shouldBe` Right ([], [SLet "a" (TArray (TyInt Signed W32) 3) (ArrayLit [Lit 1, Lit 2, Lit 3])], Lit 0)
    it "繰り返し配列リテラル" $
      parseSource "fn main() -> i64 {\nlet a = [0; 5];\n0\n}" `shouldBe` Right ([], [SLetInferred "a" (ArrayRepeat (Lit 0) 5)], Lit 0)
    it "ポインタ型の配列" $
      parseSource "fn main() -> i64 {\nlet a: [&bool; 2] = [&x, &y];\n0\n}"
        `shouldBe` Right ([], [SLet "a" (TArray (TPtr TBool) 2) (ArrayLit [AddrOf (Var "x"), AddrOf (Var "y")])], Lit 0)
    it "添字アクセス（末尾式のa[0]は代入文と区別される）" $
      parseSource "fn main() {\na[0]\n}" `shouldBe` Right ([], [], Index (Var "a") (Lit 0))
    it "要素への代入文" $
      parseSource "fn main() -> i64 {\na[i] = 1;\n0\n}" `shouldBe` Right ([], [SAssignPlace (Index (Var "a") (Var "i")) (Lit 1)], Lit 0)
    it "添字は単項演算子より強く結合する" $ do
      parseSource "fn main() {\n-a[0]\n}" `shouldBe` Right ([], [], Neg (Index (Var "a") (Lit 0)))
      parseSource "fn main() {\n&a[1]\n}" `shouldBe` Right ([], [], AddrOf (Index (Var "a") (Lit 1)))
      parseSource "fn main() {\n*p[1]\n}" `shouldBe` Right ([], [], Deref (Index (Var "p") (Lit 1)))
    it "as usize" $
      parseSource "fn main() -> usize {\nx as usize\n}" `shouldBe` Right ([], [], Cast (Var "x") TUsize)
    it "ネストした配列はエラー" $
      parseSource "fn main() {\nlet a: [[i32; 2]; 2] = 0;\n0\n}" `shouldBe` Left "nested arrays are not supported"
    it "空の配列リテラルはエラー" $
      parseSource "fn main() {\nlet a = [];\n0\n}" `shouldBe` Left "empty array literal is not supported"
    it "長さ0の配列型はエラー" $
      parseSource "fn main() {\nlet a: [i32; 0] = 0;\n0\n}" `shouldBe` Left "array length must be at least 1"

  describe "配列・usize（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "配列リテラルは要素ごとに静的オフセットへストアし、a[i]はアドレス計算＋範囲検査＋LoadInd" $
      compileSource "fn main() -> i32 {\nlet a: [i32; 2] = [1, 2];\na[1]\n}"
        `shouldBe` Right
          ( []
          , TyInt Signed W32
          , [ Push 1, Store W32 (-8), Push 2, Store W32 (-4)
            , LoadAddr (-8), Push 1, IBoundsCheck 2, Push 4, IMul W64, IAdd W64, LoadInd W32, Label ".Lfn_end0"
            ]
          )
    it "[e; N] は e を1回だけ評価し、残りの要素へコピーする（u32は読み出し後にゼロ拡張）" $
      compileSource "fn main() -> u32 {\nlet a = [5u32; 3];\na[0]\n}"
        `shouldBe` Right
          ( []
          , TyInt Unsigned W32
          , [ Push 5, Store W32 (-12), Load W32 (-12), Store W32 (-8), Load W32 (-12), Store W32 (-4)
            , LoadAddr (-12), Push 0, IBoundsCheck 3, Push 4, IMul W64, IAdd W64, LoadInd W32, IZext32, Label ".Lfn_end0"
            ]
          )
    it "要素への代入はアドレス計算＋値＋StoreInd" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\na[0] = 3;\n0\n}"
        `shouldBe` Right
          ( []
          , TyInt Signed W64
          , [ Push 1, Store W64 (-16), Push 2, Store W64 (-8)
            , LoadAddr (-16), Push 0, IBoundsCheck 2, Push 8, IMul W64, IAdd W64, Push 3, StoreInd W64
            , Push 0, Label ".Lfn_end0"
            ]
          )
    it "usizeの演算はu64と同じ命令列" $
      compileSource "fn main() -> usize {\nlet i: usize = 4;\ni / 2\n}"
        `shouldBe` Right ([], TUsize, [Push 4, Store W64 (-8), Load W64 (-8), Push 2, IDivU W64, Label ".Lfn_end0"])
    it "as usize は as u64 と同じ命令列" $
      compileSource "fn main() -> usize {\nlet x: i32 = 1;\nx as usize\n}"
        `shouldBe` Right ([], TUsize, [Push 1, Store W32 (-4), Load W32 (-4), ISext32, Label ".Lfn_end0"])
    it "配列の後に宣言した変数は配列全体の後ろに割り付けられる" $
      fmap (\(_, _, is) -> last [i | i@(Store _ _) <- is]) (compileSource "fn main() -> i64 {\nlet a = [true; 2];\nlet x: i64 = 1;\nx\n}")
        `shouldBe` Right (Store W64 (-16))

  describe "配列・usize（意味論エラー）" $ do
    let compileSource src = fmap (const ()) (tokenize src >>= parse >>= compile)
    it "要素数の不一致" $
      compileSource "fn main() -> i64 {\nlet a: [i32; 3] = [1, 2];\n0\n}" `shouldBe` Left "array length mismatch: expected 3, found 2"
    it "繰り返し回数の不一致" $
      compileSource "fn main() -> i64 {\nlet a: [i32; 3] = [1; 4];\n0\n}" `shouldBe` Left "array length mismatch: expected 3, found 4"
    it "リテラル添字の範囲外はコンパイル時エラー" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2, 3];\na[3]\n}" `shouldBe` Left "index out of bounds: the length is 3 but the index is 3"
    it "u64の添字はエラー（usizeとは別の型）" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet i: u64 = 0;\na[i]\n}" `shouldBe` Left "type mismatch: expected usize, found u64"
    it "i64の添字はエラー" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet i = 0;\na[i]\n}" `shouldBe` Left "type mismatch: expected usize, found i64"
    it "usizeとu64の混在はエラー" $
      compileSource "fn main() {\nlet i: usize = 1;\nlet j: u64 = 2;\ni + j;\n}" `shouldBe` Left "type mismatch: usize and u64"
    it "要素型の不一致" $
      compileSource "fn main() -> i64 {\nlet a: [i32; 2] = [1, true];\n0\n}" `shouldBe` Left "type mismatch: expected i32, found bool"
    it "要素への代入の型不一致" $
      compileSource "fn main() -> i64 {\nlet a = [1u32, 2u32];\na[0] = 1i64;\n0\n}" `shouldBe` Left "type mismatch: expected u32, found i64"
    it "配列リテラルの要素型の混在" $
      compileSource "fn main() {\nlet a = [1i32, 2i64];\n0\n}" `shouldBe` Left "type mismatch: i32 and i64"
    it "usizeへの単項マイナスはエラー" $
      compileSource "fn main() -> usize {\nlet i: usize = 1;\n-i\n}" `shouldBe` Left "cannot apply unary minus to unsigned type usize"
    it "配列を値として使うとエラー" $ do
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\na;\n0\n}" `shouldBe` Left "array cannot be used as a value: a"
      compileSource "fn main() -> bool {\nlet a = [1, 2];\nlet b = [1, 2];\na == b\n}" `shouldBe` Left "array cannot be used as a value: a"
    it "配列変数で配列を初期化するとエラー" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet b = a;\n0\n}" `shouldBe` Left "array must be initialized with an array literal"
    it "配列全体への代入はエラー" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\na = [3, 4];\n0\n}" `shouldBe` Left "cannot assign to array a; assign to elements instead"
    it "配列リテラルをlet初期化以外で使うとエラー" $
      compileSource "fn main() -> i64 {\n[1, 2][0]\n}" `shouldBe` Left "cannot index into a value of type [i64; 2]"
    it "配列でない値への添字はエラー" $
      compileSource "fn main() {\nlet x = 1;\nx[0]\n}" `shouldBe` Left "cannot index into a value of type i64"
    it "関数の仮引数・戻り値に配列型は使えない（注釈）" $
      compileSource "fn f(a: [i32; 2]) -> i32 { 0 }\nfn main() {\n0\n}" `shouldBe` Left "array type is not allowed in function signature (pass &[T; N] instead): f"

  describe "配列（VM・codegen）" $ do
    it "IBoundsCheckは範囲内なら添字を残す" $
      run [Push 1, IBoundsCheck 2] `shouldBe` Right 1
    it "IBoundsCheckは範囲外ならエラー（負の値は符号なしで巨大な値とみなす）" $ do
      run [Push 2, IBoundsCheck 2] `shouldBe` Left "index out of bounds"
      run [Push (-1), IBoundsCheck 2] `shouldBe` Left "index out of bounds"
    it "StoreIndで書き込んだ値をLoadIndで読み戻す" $
      run [LoadAddr (-8), Push 7, StoreInd W64, LoadAddr (-8), LoadInd W64] `shouldBe` Right 7
    it "IBoundsCheckは符号なし比較で.Lindex_errorへジャンプする" $
      codegen [] (TyInt Signed W64) [IBoundsCheck 3] `shouldContain` "jae   .Lindex_error"
    it "StoreInd W32はmovlで間接ストアする" $
      codegen [] (TyInt Signed W64) [StoreInd W32] `shouldContain` "movl  %eax, (%rcx)"
    it "usizeの末尾式は%luで出力する" $
      codegen [] TUsize [] `shouldContain` "leaq  fmtU64(%rip), %rdi"

  describe "compile + codegen + gcc（配列の結合テスト）" $ do
    it "i32配列の合計をusizeのwhileループで求める" $ do
      result <-
        compileSourceAndRun
          "fn main() -> i32 {\nlet a: [i32; 5] = [1, 2, 3, 4, 5];\nlet s: i32 = 0;\nlet i: usize = 0;\nwhile i < 5 {\ns = s + a[i];\ni = i + 1;\n}\ns\n}"
      result `shouldBe` "15"
    it "[0; 5] へ書き込んで読み戻す（i64）" $ do
      result <-
        compileSourceAndRun
          "fn main() -> i64 {\nlet a = [0; 5];\nlet i: usize = 0;\nwhile i < 5 {\na[i] = (i as i64) * 10;\ni = i + 1;\n}\na[0] + a[4]\n}"
      result `shouldBe` "40"
    it "u32配列の要素はラップアラウンドする" $ do
      result <- compileSourceAndRun "fn main() -> u32 {\nlet a = [0u32; 2];\na[1] = a[0] - 1u32;\na[1]\n}"
      result `shouldBe` "4294967295"
    it "u64配列" $ do
      result <- compileSourceAndRun "fn main() -> u64 {\nlet a = [18446744073709551615u64, 1u64];\na[0] / 2u64\n}"
      result `shouldBe` "9223372036854775807"
    it "bool配列" $ do
      result <- compileSourceAndRun "fn main() -> bool {\nlet b = [true, false, true];\nb[1] = true;\nb[0] == b[1]\n}"
      result `shouldBe` "true"
    it "ポインタ型の配列" $ do
      result <-
        compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 10;\nlet y: i32 = 20;\nlet ps: [&i32; 2] = [&x, &y];\ny = 25;\n*ps[0] + *ps[1]\n}"
      result `shouldBe` "35"
    it "&a[i] で要素のアドレスを取得し、*で読み戻す" $ do
      result <- compileSourceAndRun "fn main() -> i64 {\nlet a = [1, 2, 3];\nlet p = &a[2];\na[2] = 9;\n*p + *&a[0]\n}"
      result `shouldBe` "10"
    it "[e; N] の e は1回だけ評価される（関数呼び出し）" $ do
      result <- compileSourceAndRun "fn seven() -> i32 { 7 }\nfn main() -> i32 {\nlet a = [seven(); 3];\na[0] + a[1] + a[2]\n}"
      result `shouldBe` "21"
    it "関数内のローカル配列" $ do
      result <-
        compileSourceAndRun
          "fn sum3(x: i64, y: i64, z: i64) -> i64 {\nlet a = [x, y, z];\na[0] + a[1] + a[2]\n}\nfn main() -> i64 {\nsum3(1, 2, 3)\n}"
      result `shouldBe` "6"
    it "配列の前後の変数を壊さない" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 1;\nlet a = [0i32; 3];\nlet y: i32 = 2;\na[0] = 5;\na[2] = 7;\nx + y\n}"
      result `shouldBe` "3"
    it "usizeの最大値を%luで出力する" $ do
      result <- compileSourceAndRun "fn main() -> usize {\n18446744073709551615usize\n}"
      result `shouldBe` "18446744073709551615"
    it "-1 as usize はusizeの最大値（as u64 と同じ）" $ do
      result <- compileSourceAndRun "fn main() -> usize {\nlet x: i32 = -1;\nx as usize\n}"
      result `shouldBe` "18446744073709551615"
    it "実行時の添字の範囲外はエラーメッセージを出力して非ゼロ終了する" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> i64 {\nlet a = [1, 2, 3];\nlet i: usize = 3;\na[i]\n}"
      out `shouldBe` "index out of bounds"
      code `shouldNotBe` ExitSuccess
    it "要素への代入でも範囲外を検出する（0 - 1 のラップアラウンド）" $ do
      (code, out) <- compileSourceAndRunExit "fn main() -> i64 {\nlet a = [1, 2];\nlet i: usize = 0;\na[i - 1] = 5;\n0\n}"
      out `shouldBe` "index out of bounds"
      code `shouldNotBe` ExitSuccess

  describe "配列へのポインタ（parse）" $ do
    let parseSource src = tokenize src >>= fmap splitMain . parse
    it "配列へのポインタ型" $
      parseSource "fn main() -> i64 {\nlet p: &[i32; 3] = &a;\n0\n}"
        `shouldBe` Right ([], [SLet "p" (TPtr (TArray (TyInt Signed W32) 3)) (AddrOf (Var "a"))], Lit 0)
    it "配列へのポインタを要素とする配列型" $
      parseSource "fn main() -> i64 {\nlet ps: [&[bool; 2]; 3] = [&a; 3];\n0\n}"
        `shouldBe` Right ([], [SLet "ps" (TArray (TPtr (TArray TBool 2)) 3) (ArrayRepeat (AddrOf (Var "a")) 3)], Lit 0)
    it "(*p)[i] = v;" $
      parseSource "fn main() -> i64 {\n(*p)[i] = 1;\n0\n}"
        `shouldBe` Right ([], [SAssignPlace (Index (Deref (Var "p")) (Var "i")) (Lit 1)], Lit 0)
    it "*p = v;" $
      parseSource "fn main() -> i64 {\n*p = 1;\n0\n}" `shouldBe` Right ([], [SAssignPlace (Deref (Var "p")) (Lit 1)], Lit 0)
    it "末尾式の (*p)[0] / *p は代入文と誤認されない" $ do
      parseSource "fn main() {\n(*p)[0]\n}" `shouldBe` Right ([], [], Index (Deref (Var "p")) (Lit 0))
      parseSource "fn main() {\n*p\n}" `shouldBe` Right ([], [], Deref (Var "p"))
    it "末尾式のパースエラーはそのまま報告される" $
      parseSource "fn main() {\nlet x = 1;\n(x\n}" `shouldBe` Left "expected closing parenthesis"

  describe "配列へのポインタ（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "&a は配列先頭のアドレス" $
      compileSource "fn main() -> &[i64; 2] {\nlet a = [1, 2];\n&a\n}"
        `shouldBe` Right ([], TPtr (TArray (TyInt Signed W64) 2), [Push 1, Store W64 (-16), Push 2, Store W64 (-8), LoadAddr (-16), Label ".Lfn_end0"])
    it "p[i] はポインタの値を先頭アドレスとして自動デリファレンスする" $
      fmap (\(_, _, is) -> drop 5 is) (compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\np[1]\n}")
        `shouldBe` Right [Store W64 (-24), Load W64 (-24), Push 1, IBoundsCheck 2, Push 8, IMul W64, IAdd W64, LoadInd W64, Label ".Lfn_end0"]
    it "(*p)[i] は p[i] と同じ命令列" $
      fmap (\(_, _, is) -> is) (compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\n(*p)[1]\n}")
        `shouldBe` fmap (\(_, _, is) -> is) (compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\np[1]\n}")
    it "*p = v はポインタの値へ StoreInd する" $
      fmap (\(_, _, is) -> is) (compileSource "fn main() -> i32 {\nlet x: i32 = 1;\nlet p = &x;\n*p = 2;\nx\n}")
        `shouldBe` Right
          [Push 1, Store W32 (-4), LoadAddr (-4), Store W64 (-12), Load W64 (-12), Push 2, StoreInd W32, Load W32 (-4), Label ".Lfn_end0"]

  describe "配列へのポインタ（意味論エラー）" $ do
    let compileSource src = fmap (const ()) (tokenize src >>= parse >>= compile)
    it "ローカル変数へのポインタを末尾式で返すとエラー" $
      compileSource "fn f() -> &i64 { let x: i64 = 1; &x }\nfn main() -> i64 {\n0\n}" `shouldBe` Left "cannot return reference to local variable: x"
    it "ローカル配列へのポインタを返すとエラー" $
      compileSource "fn f() -> &[i64; 2] { let a = [1, 2]; &a }\nfn main() -> i64 {\n0\n}" `shouldBe` Left "cannot return reference to local variable: a"
    it "ローカル配列の要素へのポインタをreturnで返すとエラー" $
      compileSource "fn f(p: &i64) -> &i64 { let a = [1, 2]; if true { return &a[1]; } p }\nfn main() -> i64 {\n0\n}"
        `shouldBe` Left "cannot return reference to local variable: a"
    it "return文でもローカルへのポインタを検出する" $
      compileSource "fn f(p: &i64) -> &i64 { let x: i64 = 1; if true { return &x; } p }\nfn main() -> i64 {\n0\n}"
        `shouldBe` Left "cannot return reference to local variable: x"
    it "仮引数へのポインタを返すとエラー" $
      compileSource "fn f(x: i64) -> &i64 { &x }\nfn main() -> i64 {\n0\n}" `shouldBe` Left "cannot return reference to local variable: x"
    it "引数のポインタ経由の &*p / &p[i] / p は返せる" $ do
      compileSource "fn f(p: &i64) -> &i64 { &*p }\nfn main() -> i64 {\n0\n}" `shouldBe` Right ()
      compileSource "fn f(p: &[i64; 2]) -> &i64 { &p[1] }\nfn main() -> i64 {\n0\n}" `shouldBe` Right ()
      compileSource "fn f(p: &[i64; 2]) -> &[i64; 2] { p }\nfn main() -> i64 {\n0\n}" `shouldBe` Right ()
    it "*p（配列）を値として使うとエラー" $ do
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\n*p;\n0\n}" `shouldBe` Left "array cannot be used as a value: dereferenced array pointer"
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\nlet b = *p;\n0\n}" `shouldBe` Left "array must be initialized with an array literal"
    it "*p（配列）への代入はエラー" $
      compileSource "fn main() -> i64 {\nlet a = [1, 2];\nlet p = &a;\n*p = [3, 4];\n0\n}" `shouldBe` Left "cannot assign to array; assign to elements instead"
    it "場所式でない左辺への代入はエラー" $
      compileSource "fn main() -> i64 {\n5 = 1;\n0\n}" `shouldBe` Left "invalid left-hand side of assignment"
    it "長さが異なる配列へのポインタは渡せない" $
      compileSource "fn f(p: &[i64; 3]) -> i64 { p[0] }\nfn main() -> i64 {\nlet a = [1, 2, 3, 4];\nf(&a)\n}"
        `shouldBe` Left "type mismatch: expected &[i64; 3], found &[i64; 4]"
    it "要素型が異なる配列へのポインタは渡せない" $
      compileSource "fn f(p: &[i64; 2]) -> i64 { p[0] }\nfn main() -> i64 {\nlet a = [1i32, 2i32];\nf(&a)\n}"
        `shouldBe` Left "type mismatch: expected &[i64; 2], found &[i32; 2]"
    it "配列へのポインタ経由のリテラル添字もコンパイル時に範囲検査する" $
      compileSource "fn f(p: &[i64; 2]) -> i64 { p[2] }\nfn main() -> i64 {\n0\n}" `shouldBe` Left "index out of bounds: the length is 2 but the index is 2"
    it "配列でないポインタへの添字はエラー" $
      compileSource "fn main() {\nlet x: i64 = 1;\nlet p = &x;\np[0]\n}" `shouldBe` Left "cannot index into a value of type &i64"

  describe "compile + codegen + gcc（配列へのポインタの結合テスト）" $ do
    it "出力引数で呼び出し元の配列を書き換える（p[i] / (*p)[i]）" $ do
      result <-
        compileSourceAndRun
          "fn fill(p: &[i32; 3], v: i32) -> i32 {\np[0] = v;\np[1] = v + 1;\n(*p)[2] = v + 2;\n0\n}\nfn main() -> i32 {\nlet a = [0i32; 3];\nlet d = fill(&a, 5);\na[0] + a[1] + a[2]\n}"
      result `shouldBe` "18"
    it "ポインタ経由で合計を求める" $ do
      result <-
        compileSourceAndRun
          "fn sum(p: &[i64; 4]) -> i64 {\nlet s = 0;\nlet i: usize = 0;\nwhile i < 4 {\ns = s + p[i];\ni = i + 1;\n}\ns\n}\nfn main() -> i64 {\nlet a = [1, 2, 3, 4];\nsum(&a)\n}"
      result `shouldBe` "10"
    it "引数で受け取ったポインタを返し、呼び出し側で添字アクセスする" $ do
      result <-
        compileSourceAndRun
          "fn pick(a: &[i32; 3], b: &[i32; 3], first: bool) -> &[i32; 3] {\nif first {\nreturn a;\n}\nb\n}\nfn main() -> i32 {\nlet x = [1i32; 3];\nlet y = [9i32; 3];\nlet r = pick(&x, &y, false);\nx[0] + r[2] + pick(&x, &y, true)[1]\n}"
      result `shouldBe` "11"
    it "u32配列へのポインタ（ゼロ拡張の維持）" $ do
      result <-
        compileSourceAndRun
          "fn dec(p: &[u32; 2]) -> u32 {\np[1] = p[0] - 1u32;\np[1]\n}\nfn main() -> u32 {\nlet a = [0u32, 0u32];\ndec(&a)\n}"
      result `shouldBe` "4294967295"
    it "bool配列へのポインタ" $ do
      result <- compileSourceAndRun "fn same(b: &[bool; 2]) -> bool { b[0] == b[1] }\nfn main() -> bool {\nlet t = [true, false];\nt[1] = true;\nsame(&t)\n}"
      result `shouldBe` "true"
    it "配列へのポインタを要素とする配列" $ do
      result <-
        compileSourceAndRun "fn main() -> i64 {\nlet a = [1, 2];\nlet b = [3, 4];\nlet ps = [&a, &b];\nps[1][0] = 30;\nb[0] + ps[0][1]\n}"
      result `shouldBe` "32"
    it "要素へのポインタを返し、*p = v で書き込む" $ do
      result <-
        compileSourceAndRun "fn at(p: &[i64; 3], i: usize) -> &i64 { &p[i] }\nfn main() -> i64 {\nlet a = [1, 2, 3];\nlet q = at(&a, 2);\n*q = 40;\na[2] + *at(&a, 0)\n}"
      result `shouldBe` "41"
    it "スカラーの *p = v" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = 1;\nlet p = &x;\n*p = *p + 41;\nx\n}"
      result `shouldBe` "42"
    it "末尾式が配列へのポインタなら%p形式で出力する" $ do
      result <- compileSourceAndRun "fn main() -> &[i64; 2] {\nlet a = [1, 2];\n&a\n}"
      take 2 result `shouldBe` "0x"
    it "ポインタ経由の範囲外アクセスは実行時エラー" $ do
      (code, out) <-
        compileSourceAndRunExit "fn get(p: &[i64; 2], i: usize) -> i64 { p[i] }\nfn main() -> i64 {\nlet a = [1, 2];\nget(&a, 2usize)\n}"
      out `shouldBe` "index out of bounds"
      code `shouldNotBe` ExitSuccess

  describe "ユニット型・式文（parse）" $ do
    let parseSrc src = tokenize src >>= fmap splitMain . parse
    it "() リテラル" $
      parseSrc "fn main() {\n()\n}" `shouldBe` Right ([], [], UnitLit)
    it "() 型注釈（&() / [(); N] も可）" $
      parseSrc "fn main() {\nlet u: () = ();\nlet p: &() = &u;\nlet a: [(); 2] = [(), ()];\nu\n}"
        `shouldBe` Right
          ( []
          , [ SLet "u" TUnit UnitLit
            , SLet "p" (TPtr TUnit) (AddrOf (Var "u"))
            , SLet "a" (TArray TUnit 2) (ArrayLit [UnitLit, UnitLit])
            ]
          , Var "u"
          )
    it "末尾式を省略したfnは末尾式 () に脱糖する" $
      parseSrc "fn f() {\nlet x = 1;\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [] TUnit ([SLetInferred "x" (Lit 1)], UnitLit)], [], Lit 2)
    it "return; は return () に脱糖する" $
      parseSrc "fn f() -> () {\nreturn;\n}\nfn main() -> i64 {\n2\n}"
        `shouldBe` Right ([FnDecl "f" [] TUnit ([SReturn UnitLit], UnitLit)], [], Lit 2)
    it "式文" $
      parseSrc "fn main() {\nf(1);\n1 + 2;\nx\n}"
        `shouldBe` Right ([], [SExpr (Call "f" [Lit 1]), SExpr (Add (Lit 1) (Lit 2))], Var "x")
    it "mainの末尾式を省略すると末尾式 () に脱糖する" $
      parseSrc "fn main() {\nlet x = 1;\nf(x);\n}"
        `shouldBe` Right ([], [SLetInferred "x" (Lit 1), SExpr (Call "f" [Var "x"])], UnitLit)
    it "() へのキャストはエラー" $
      parseSrc "fn main() {\n1 as ()\n}" `shouldBe` Left "invalid cast target: only integer types (i32/i64/u32/u64/usize) are allowed"

  describe "ユニット型・式文（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
        finalTypeOf src = (\(_, t, _) -> t) <$> compileSource src
    it "末尾式を省略したmainの戻り値型は ()" $
      finalTypeOf "fn main() {\nlet x = 1;\n}" `shouldBe` Right TUnit
    it "戻り値型を省略した末尾式なしのfnの戻り値型は ()" $
      finalTypeOf "fn f() {\nlet x = 1;\n}\nfn main() {\nf()\n}" `shouldBe` Right TUnit
    it "戻り値型を省略し return; のみのfnの戻り値型は ()" $
      finalTypeOf "fn f(x: i64) {\nif x > 0 {\nreturn;\n}\n}\nfn main() {\nf(1)\n}" `shouldBe` Right TUnit
    it "() を返すfnの呼び出しで初期化した変数の型は ()" $
      finalTypeOf "fn f() {}\nfn main() {\nlet u = f();\nu\n}" `shouldBe` Right TUnit
    it "return文の値・末尾式の型は戻り値型の注釈に従う" $
      finalTypeOf "fn f(x: i64) -> i32 {\nif x > 0 {\nreturn 1i32;\n}\n2\n}\nfn main() -> i32 {\nf(1)\n}" `shouldBe` Right (TyInt Signed W32)
    it "式文は値を捨てる（IPop）" $
      (\(_, _, is) -> is) <$> compileSource "fn main() -> i64 {\n1;\n2\n}"
        `shouldBe` Right [Push 1, IPop, Push 2, Label ".Lfn_end0"]
    it "式文はVMでも値を捨てる" $
      (((\(_, _, is) -> is) <$> compileSource "fn main() -> i64 {\nlet x = 1;\nx + 1;\nx\n}") >>= run)
        `shouldBe` Right 1

  describe "ユニット型・式文（意味論エラー）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "-> i64 なのに末尾式を省略するとエラー" $
      compileSource "fn f() -> i64 {\nlet x = 1;\n}\nfn main() -> i64 {\nf()\n}"
        `shouldBe` Left "type mismatch: expected i64, found ()"
    it "-> i64 なのに return; するとエラー" $
      compileSource "fn f() -> i64 {\nreturn;\n}\nfn main() -> i64 {\nf()\n}"
        `shouldBe` Left "type mismatch: expected i64, found ()"
    it "-> () なのに値を return するとエラー" $
      compileSource "fn f() -> () {\nreturn 1i64;\n}\nfn main() {\nf()\n}"
        `shouldBe` Left "type mismatch: expected (), found i64"
    it "戻り値型を省略した（() を返す）関数で値を return するとエラー" $
      compileSource "fn f(x: i64) {\nif x > 0 {\nreturn 1i64;\n}\n}\nfn main() {\nf(1)\n}"
        `shouldBe` Left "type mismatch: expected (), found i64"
    it "() 型の変数に整数を代入するとエラー" $
      compileSource "fn main() {\nlet x: () = 5;\nx\n}" `shouldBe` Left "type mismatch: expected (), found integer literal"
    it "整数型の変数に () を代入するとエラー" $
      compileSource "fn main() -> i64 {\nlet x: i64 = ();\nx\n}" `shouldBe` Left "type mismatch: expected i64, found ()"
    it "() 同士の算術はエラー" $
      compileSource "fn main() {\n() + ()\n}" `shouldBe` Left "type mismatch: expected (), found arithmetic expression"
    it "() 同士の大小比較はエラー" $
      compileSource "fn main() -> bool {\n() < ()\n}" `shouldBe` Left "type mismatch: expected i32 or i64, found ()"
    it "if の条件に () はエラー" $
      compileSource "fn main() -> i64 {\nif () {\n}\n1\n}" `shouldBe` Left "type mismatch: expected bool, found ()"

  describe "compile + codegen + gcc（ユニット型・式文の結合テスト）" $ do
    it "() を返すfnを式文で呼び、ポインタ経由で書き込む（return; による早期脱出）" $ do
      result <-
        compileSourceAndRun
          "fn set(p: &i64, v: i64) {\nif v < 0 {\nreturn;\n}\n*p = v;\n}\nfn main() -> i64 {\nlet x: i64 = 1;\nset(&x, 5);\nset(&x, -1);\nx\n}"
      result `shouldBe` "5"
    it "-> () を明示したfnから別の () のfnを末尾式で呼ぶ" $ do
      result <-
        compileSourceAndRun
          "fn inc(p: &i64) {\n*p = *p + 1;\n}\nfn twice(p: &i64) -> () {\ninc(p);\ninc(p)\n}\nfn main() -> i64 {\nlet x = 40;\ntwice(&x);\nx\n}"
      result `shouldBe` "42"
    it "() 型の変数・ポインタ・比較" $ do
      result <- compileSourceAndRun "fn noop() {}\nfn main() -> bool {\nlet u = noop();\nlet w: () = ();\nlet p = &w;\nu == *p\n}"
      result `shouldBe` "true"
    it "() 型の仮引数" $ do
      result <- compileSourceAndRun "fn f(u: (), x: i64) -> i64 {\nx + 1\n}\nfn main() -> i64 {\nf((), 41)\n}"
      result `shouldBe` "42"
    it "式文で () 以外の値を捨てる（式中の呼び出しのアライメントを含む）" $ do
      result <-
        compileSourceAndRun "fn add(p: &i64, v: i64) -> i64 {\n*p = *p + v;\n*p\n}\nfn main() -> i64 {\nlet x = 0;\nadd(&x, 1);\n1 + add(&x, 2);\nx\n}"
      result `shouldBe` "3"
    it "戻り値が () のmainは何も出力しない" $ do
      (code, out) <- compileSourceAndRunExit "fn main() {\nlet x = 1;\nx = 2;\n}"
      out `shouldBe` ""
      code `shouldBe` ExitSuccess

  describe "明示的なmain（parse）" $ do
    let parseProgram src = tokenize src >>= parse
    it "トップレベルはfn定義の列" $
      parseProgram "fn f() -> i64 { 1 }\nfn main() -> i64 { f() }"
        `shouldBe` Right [FnDecl "f" [] (TyInt Signed W64) ([], Lit 1), FnDecl "main" [] (TyInt Signed W64) ([], Call "f" [])]
    it "空のプログラムはfn定義0個（mainの有無はcompile側で検査）" $
      parseProgram "" `shouldBe` Right []
    it "トップレベルの文はエラー" $
      parseProgram "let x = 1;\nfn main() { x }" `shouldBe` Left "expected 'fn' at top level, got: TLet"
    it "トップレベルの式はエラー" $
      parseProgram "fn main() { 1 }\n2" `shouldBe` Left "expected 'fn' at top level, got: TInt 2"

  describe "明示的なmain（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
        finalTypeOf src = (\(_, t, _) -> t) <$> compileSource src
    it "mainが無いとエラー" $
      compileSource "fn f() { 1 }" `shouldBe` Left "missing function: main"
    it "空のプログラムはmainが無いのでエラー" $
      compileSource "" `shouldBe` Left "missing function: main"
    it "mainの重複定義はエラー" $
      compileSource "fn main() { 1 }\nfn main() { 2 }" `shouldBe` Left "function already declared: main"
    it "mainに引数があるとエラー" $
      compileSource "fn main(x: i64) { x }" `shouldBe` Left "main function must not take parameters"
    it "mainの呼び出しはエラー（再帰）" $
      compileSource "fn main() -> i64 { main() }" `shouldBe` Left "cannot call main"
    it "mainの呼び出しはエラー（他の関数から）" $
      compileSource "fn f() -> i64 { main() }\nfn main() -> i64 { f() }" `shouldBe` Left "cannot call main"
    it "mainの呼び出しはエラー（式文）" $
      compileSource "fn main() {\nmain();\n}" `shouldBe` Left "cannot call main"
    it "mainの戻り値型は注釈のとおり" $
      finalTypeOf "fn main() -> i32 { 1i32 }" `shouldBe` Right (TyInt Signed W32)
    it "mainの戻り値型の注釈に合わせて無型リテラルの型が決まる" $
      finalTypeOf "fn main() -> u32 {\nif true {\nreturn 1;\n}\n2\n}" `shouldBe` Right (TyInt Unsigned W32)
    it "戻り値型を省略したmainの戻り値型は ()" $
      finalTypeOf "fn main() {}" `shouldBe` Right TUnit
    it "戻り値型を省略したmainが値を返すとエラー（推論はしない）" $
      compileSource "fn main() { 42 }" `shouldSatisfy` isLeft
    it "戻り値型を省略した関数が値を返すとエラー（推論はしない）" $
      compileSource "fn f() { 1 }\nfn main() -> i64 { 0 }" `shouldSatisfy` isLeft
    it "mainの戻り値型の注釈と末尾式が食い違うとエラー" $
      compileSource "fn main() -> i32 { 1i64 }" `shouldBe` Left "type mismatch: expected i32, found i64"
    it "mainのreturn文と戻り値型の注釈が食い違うとエラー" $
      compileSource "fn main() -> i64 {\nif true {\nreturn 1i32;\n}\n2i64\n}" `shouldBe` Left "type mismatch: expected i64, found i32"
    it "mainはローカル変数へのポインタを返せる（ダングリング検査の対象外）" $
      finalTypeOf "fn main() -> &i64 {\nlet x = 1;\n&x\n}" `shouldBe` Right (TPtr (TyInt Signed W64))
    it "main以外の関数の命令列にmainは含まれない" $
      (\(fns, _, _) -> map fst fns) <$> compileSource "fn f() -> i64 { 1 }\nfn main() -> i64 { f() }\nfn g() -> i64 { 2 }"
        `shouldBe` Right ["f", "g"]

  describe "compile + codegen + gcc（明示的なmainの結合テスト）" $ do
    it "mainからの早期return" $ do
      result <-
        compileSourceAndRun
          "fn add(a: i64, b: i64) -> i64 { a + b }\nfn main() -> i64 {\nlet x = add(1, 2);\nif x > 2 {\nreturn x * 10;\n}\nx\n}"
      result `shouldBe` "30"
    it "早期returnしない場合は末尾式の値を表示する" $ do
      result <-
        compileSourceAndRun "fn main() -> i64 {\nlet x = 1;\nif x > 2 {\nreturn x * 10;\n}\nx\n}"
      result `shouldBe` "1"
    it "mainの戻り値型を明示する（-> i32）" $ do
      result <- compileSourceAndRun "fn main() -> i32 {\nlet x: i32 = -7;\nx\n}"
      result `shouldBe` "-7"
    it "mainのreturn;（戻り値 ()）は何も出力しない" $ do
      (code, out) <- compileSourceAndRunExit "fn main() {\nlet x = 1;\nif x == 1 {\nreturn;\n}\n}"
      out `shouldBe` ""
      code `shouldBe` ExitSuccess
    it "mainがローカル配列へのポインタを返すと%p形式で表示する" $ do
      result <- compileSourceAndRun "fn main() -> &[i64; 2] {\nlet a = [1, 2];\nif a[0] == 1 {\nreturn &a;\n}\n&a\n}"
      take 2 result `shouldBe` "0x"

-- パース結果を (main以外のfn定義, mainの文の列, mainの末尾式) に分解する（main が無ければテスト失敗）
splitMain :: Program -> ([FnDecl], [Stmt], Expr)
splitMain prog =
  case [body | FnDecl "main" _ _ body <- prog] of
    [(stmts, e)] -> ([d | d@(FnDecl name _ _ _) <- prog, name /= "main"], stmts, e)
    _ -> error "splitMain: expected exactly one main"

-- トークン列を main の本体として包んでパースする（文・式単位のパースのテスト用）
parseMain :: [Token] -> Either String ([FnDecl], [Stmt], Expr)
parseMain toks = splitMain <$> parse ([TFn, TIdent "main", TLParen, TRParen, TLBrace] ++ toks ++ [TRBrace])

compileAndRun :: Type -> Expr -> IO String
compileAndRun retTy expr =
  withSystemTempDirectory "hs006" $ \tmpDir -> do
    let asmPath = tmpDir </> "out.s"
        binPath = tmpDir </> "out"
    case compile [FnDecl "main" [] retTy ([], expr)] of
      Left err -> error ("compile failed: " ++ err)
      Right (fns, finalType, instrs) -> do
        writeFile asmPath (codegen fns finalType instrs)
        _ <- readProcess "gcc" [asmPath, "-o", binPath] ""
        output <- readProcess binPath [] ""
        return (takeWhile (/= '\n') output)

-- ソース文字列からフルパイプライン（tokenize/parse/compile）を通して実行する。
buildAndRun :: (FilePath -> IO a) -> String -> IO a
buildAndRun runBinary src =
  withSystemTempDirectory "hs006" $ \tmpDir -> do
    let asmPath = tmpDir </> "out.s"
        binPath = tmpDir </> "out"
    case tokenize src >>= parse >>= compile of
      Left err -> error ("compile failed: " ++ err)
      Right (fns, finalType, instrs) -> do
        writeFile asmPath (codegen fns finalType instrs)
        _ <- readProcess "gcc" [asmPath, "-o", binPath] ""
        runBinary binPath

compileSourceAndRun :: String -> IO String
compileSourceAndRun = buildAndRun (\binPath -> takeWhile (/= '\n') <$> readProcess binPath [] "")

-- 非ゼロ終了するプログラム用: readProcess は非ゼロ終了時に例外を投げるため使えない。
compileSourceAndRunExit :: String -> IO (ExitCode, String)
compileSourceAndRunExit = buildAndRun $ \binPath -> do
  (code, out, _err) <- readProcessWithExitCode binPath [] ""
  return (code, takeWhile (/= '\n') out)
