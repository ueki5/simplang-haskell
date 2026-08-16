module Main where

import Compiler (Instr (..), compile, run)
import Data.Either (isLeft, isRight)
import Parser (Expr (..), FnDecl (..), Stmt (..), Token (..), Type (..), Width (..), parse, tokenize)
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
    it "to_i64/to_i32予約語を変換する" $
      tokenize "to_i64 to_i32" `shouldBe` Right [TToI64, TToI32]
    it "to_i64/to_i32で始まる識別子はTIdentのまま" $
      tokenize "to_i64x to_i32_foo" `shouldBe` Right [TIdent "to_i64x", TIdent "to_i32_foo"]
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

  describe "parse" $ do
    it "整数リテラル" $
      parse [TInt 5] `shouldBe` Right ([], [], Lit 5)
    it "加算" $
      parse [TInt 1, TPlus, TInt 2] `shouldBe` Right ([], [], Add (Lit 1) (Lit 2))
    it "減算" $
      parse [TInt 3, TMinus, TInt 1] `shouldBe` Right ([], [], Sub (Lit 3) (Lit 1))
    it "乗算が加算より優先される" $
      parse [TInt 1, TPlus, TInt 2, TStar, TInt 3]
        `shouldBe` Right ([], [], Add (Lit 1) (Mul (Lit 2) (Lit 3)))
    it "括弧で優先度を変える" $
      parse [TLParen, TInt 1, TPlus, TInt 2, TRParen, TStar, TInt 3]
        `shouldBe` Right ([], [], Mul (Add (Lit 1) (Lit 2)) (Lit 3))
    it "単項マイナス" $
      parse [TMinus, TInt 5] `shouldBe` Right ([], [], Neg (Lit 5))
    it "空入力はエラー" $
      parse [] `shouldBe` Left "unexpected end of input"
    it "変数参照" $
      parse [TIdent "x"] `shouldBe` Right ([], [], Var "x")
    it "let宣言（i64）" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" (TyInt W64) (Lit 1)], Var "x")
    it "let宣言（i32）" $
      parse [TLet, TIdent "x", TColon, TIdent "i32", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" (TyInt W32) (Lit 1)], Var "x")
    it "let宣言（bool）" $
      parse [TLet, TIdent "x", TColon, TIdent "bool", TAssign, TTrue, TSemicolon, TIdent "x"]
        `shouldBe` Right ([], [SLet "x" TBool (BoolLit True)], Var "x")
    it "代入文" $
      parse
        [ TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TIdent "x", TAssign, TInt 2, TSemicolon
        , TIdent "x"
        ]
        `shouldBe` Right ([], [SLet "x" (TyInt W64) (Lit 1), SAssign "x" (Lit 2)], Var "x")
    it "let宣言でコロンが無ければエラー" $
      parse [TLet, TIdent "x", TAssign, TInt 1, TSemicolon, TIdent "x"] `shouldSatisfy` isLeft
    it "let宣言で型がi32/i64/bool以外ならエラー" $
      parse [TLet, TIdent "x", TColon, TIdent "i16", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Left "unsupported type: i16"
    it "let宣言で=が無ければエラー" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TInt 1, TSemicolon, TIdent "x"]
        `shouldSatisfy` isLeft
    it "文の終端に;が無ければエラー" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TIdent "x"]
        `shouldSatisfy` isLeft
    it "末尾式が無い（文だけの）プログラムはエラー" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon]
        `shouldSatisfy` isLeft
    it "末尾式の後に;を書くとエラー" $
      parse [TInt 1, TSemicolon] `shouldSatisfy` isLeft
    it "boolリテラル（true）" $
      parse [TTrue] `shouldBe` Right ([], [], BoolLit True)
    it "boolリテラル（false）" $
      parse [TFalse] `shouldBe` Right ([], [], BoolLit False)
    it "等価演算子" $
      parse [TInt 1, TEq, TInt 2] `shouldBe` Right ([], [], Eq (Lit 1) (Lit 2))
    it "非等価演算子" $
      parse [TInt 1, TNeq, TInt 2] `shouldBe` Right ([], [], Neq (Lit 1) (Lit 2))
    it "小なり演算子" $
      parse [TInt 1, TLt, TInt 2] `shouldBe` Right ([], [], Lt (Lit 1) (Lit 2))
    it "以下演算子" $
      parse [TInt 1, TLe, TInt 2] `shouldBe` Right ([], [], Le (Lit 1) (Lit 2))
    it "大なり演算子" $
      parse [TInt 1, TGt, TInt 2] `shouldBe` Right ([], [], Gt (Lit 1) (Lit 2))
    it "以上演算子" $
      parse [TInt 1, TGe, TInt 2] `shouldBe` Right ([], [], Ge (Lit 1) (Lit 2))
    it "比較演算子は加減算より優先順位が低い" $
      parse [TInt 1, TPlus, TInt 2, TLt, TInt 3]
        `shouldBe` Right ([], [], Lt (Add (Lit 1) (Lit 2)) (Lit 3))
    it "比較演算子は等価演算子より強く結合する" $
      parse [TInt 1, TLt, TInt 2, TEq, TTrue]
        `shouldBe` Right ([], [], Eq (Lt (Lit 1) (Lit 2)) (BoolLit True))
    it "括弧内の比較演算子をパースできる" $
      parse [TLParen, TInt 1, TLt, TInt 2, TRParen]
        `shouldBe` Right ([], [], Lt (Lit 1) (Lit 2))
    it "否定演算子" $
      parse [TBang, TTrue] `shouldBe` Right ([], [], Not (BoolLit True))
    it "等価演算子は加減算より優先順位が低い" $
      parse [TInt 1, TPlus, TInt 2, TEq, TInt 3]
        `shouldBe` Right ([], [], Eq (Add (Lit 1) (Lit 2)) (Lit 3))
    it "否定演算子は等価演算子より強く結合する" $
      parse [TBang, TTrue, TEq, TFalse]
        `shouldBe` Right ([], [], Eq (Not (BoolLit True)) (BoolLit False))
    it "括弧内の等価演算子をパースできる" $
      parse [TLParen, TInt 1, TEq, TInt 1, TRParen]
        `shouldBe` Right ([], [], Eq (Lit 1) (Lit 1))
    it "to_i64演算子（括弧なし）" $
      parse [TToI64, TInt 64] `shouldBe` Right ([], [], ToI64 (Lit 64))
    it "to_i32演算子（括弧なし）" $
      parse [TToI32, TInt 64] `shouldBe` Right ([], [], ToI32 (Lit 64))
    it "to_i64演算子（括弧あり）は括弧なしと同じASTになる" $
      parse [TToI64, TLParen, TIdent "x", TRParen]
        `shouldBe` Right ([], [], ToI64 (Var "x"))
    it "to_i32とto_i64は入れ子にできる" $
      parse [TToI32, TToI64, TIdent "x"]
        `shouldBe` Right ([], [], ToI32 (ToI64 (Var "x")))
    it "単項マイナスはto_i32より強く結合する" $
      parse [TToI32, TMinus, TInt 5]
        `shouldBe` Right ([], [], ToI32 (Neg (Lit 5)))
    it "空ブロック" $
      parse [TLBrace, TRBrace, TInt 5]
        `shouldBe` Right ([], [SBlock []], Lit 5)
    it "ブロック内にlet文を含む" $
      parse
        [ TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SBlock [SLet "x" (TyInt W64) (Lit 1)]], Lit 2)
    it "ネストしたブロック" $
      parse [TLBrace, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SBlock []]], Lit 1)
    it "閉じ括弧がないブロックはエラー" $
      parse [TLBrace] `shouldBe` Left "expected closing brace"
    it "if文（elseなし）" $
      parse [TIf, TTrue, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SIf [(BoolLit True, [])] Nothing], Lit 1)
    it "if-else文" $
      parse [TIf, TTrue, TLBrace, TRBrace, TElse, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SIf [(BoolLit True, [])] (Just [])], Lit 1)
    it "if-else if-else文（複数のelse if）" $
      parse
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
      parse
        [ TIf, TTrue, TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SIf [(BoolLit True, [SLet "x" (TyInt W64) (Lit 1)])] Nothing], Lit 2)
    it "if文はブロック文としてネストできる" $
      parse [TLBrace, TIf, TTrue, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SIf [(BoolLit True, [])] Nothing]], Lit 1)
    it "if文の条件式が無ければエラー" $
      parse [TIf, TLBrace, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "if文で閉じ波括弧がなければエラー" $
      parse [TIf, TTrue, TLBrace, TInt 1] `shouldBe` Left "expected closing brace"
    it "elseの後にifも{もなければエラー" $
      parse [TIf, TTrue, TLBrace, TRBrace, TElse, TInt 1] `shouldSatisfy` isLeft
    it "elseで入力が終わるとエラー" $
      parse [TIf, TTrue, TLBrace, TRBrace, TElse] `shouldSatisfy` isLeft
    it "while文" $
      parse [TWhile, TTrue, TLBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SWhile (BoolLit True) []], Lit 1)
    it "while文の本体にlet文とbreak文を含む" $
      parse
        [ TWhile, TTrue, TLBrace
        , TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TBreak, TSemicolon
        , TRBrace
        , TInt 2
        ]
        `shouldBe` Right ([], [SWhile (BoolLit True) [SLet "x" (TyInt W64) (Lit 1), SBreak]], Lit 2)
    it "continue文" $
      parse [TWhile, TTrue, TLBrace, TContinue, TSemicolon, TRBrace, TInt 1]
        `shouldBe` Right ([], [SWhile (BoolLit True) [SContinue]], Lit 1)
    it "while文はブロック文としてネストできる" $
      parse [TLBrace, TWhile, TTrue, TLBrace, TRBrace, TRBrace, TInt 1]
        `shouldBe` Right ([], [SBlock [SWhile (BoolLit True) []]], Lit 1)
    it "while文の条件式が無ければエラー" $
      parse [TWhile, TLBrace, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "while文で閉じ波括弧がなければエラー" $
      parse [TWhile, TTrue, TLBrace, TInt 1] `shouldBe` Left "expected closing brace"
    it "break文に;が無ければエラー" $
      parse [TWhile, TTrue, TLBrace, TBreak, TRBrace, TInt 1] `shouldSatisfy` isLeft
    it "continue文に;が無ければエラー" $
      parse [TWhile, TTrue, TLBrace, TContinue, TRBrace, TInt 1] `shouldSatisfy` isLeft

  describe "parse（fn定義・呼び出し・return）" $ do
    let parseSrc src = tokenize src >>= parse
    it "引数無しのfn定義" $
      parseSrc "fn f() -> i64 {\n1\n}\n2"
        `shouldBe` Right ([FnDecl "f" [] (TyInt W64) ([], Lit 1)], [], Lit 2)
    it "引数1個のfn定義" $
      parseSrc "fn f(a: i64) -> i64 {\na\n}\n2"
        `shouldBe` Right ([FnDecl "f" [("a", TyInt W64)] (TyInt W64) ([], Var "a")], [], Lit 2)
    it "引数複数個のfn定義（型が混在してもパースできる。型検査は別）" $
      parseSrc "fn f(a: i64, b: i32, c: bool) -> i64 {\na\n}\n2"
        `shouldBe` Right
          ( [FnDecl "f" [("a", TyInt W64), ("b", TyInt W32), ("c", TBool)] (TyInt W64) ([], Var "a")]
          , []
          , Lit 2
          )
    it "fn本体は文の列＋末尾式を持てる" $
      parseSrc "fn f() -> i64 {\nlet x: i64 = 1;\nx\n}\n2"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt W64) ([SLet "x" (TyInt W64) (Lit 1)], Var "x")]
          , []
          , Lit 2
          )
    it "fn定義は複数並べられる" $
      parseSrc "fn f() -> i64 {\n1\n}\nfn g() -> i64 {\n2\n}\n3"
        `shouldBe` Right
          ( [ FnDecl "f" [] (TyInt W64) ([], Lit 1)
            , FnDecl "g" [] (TyInt W64) ([], Lit 2)
            ]
          , []
          , Lit 3
          )
    it "fn定義は暗黙main本体の文と自由に混在できる" $
      parseSrc "let a: i64 = 1;\nfn f() -> i64 {\n1\n}\nlet b: i64 = 2;\na + b"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt W64) ([], Lit 1)]
          , [SLet "a" (TyInt W64) (Lit 1), SLet "b" (TyInt W64) (Lit 2)]
          , Add (Var "a") (Var "b")
          )
    it "fn本体に末尾式が無ければエラー（ユニット型は採用しないため）" $
      parseSrc "fn f() -> i64 {\nlet x: i64 = 1;\n}\n2" `shouldSatisfy` isLeft
    it "戻り値の型（->）が無ければエラー" $
      parseSrc "fn f() {\n1\n}\n2" `shouldSatisfy` isLeft
    it "引数無しの呼び出し式" $
      parseSrc "f()" `shouldBe` Right ([], [], Call "f" [])
    it "引数1個の呼び出し式" $
      parseSrc "f(1)" `shouldBe` Right ([], [], Call "f" [Lit 1])
    it "引数複数個の呼び出し式（カンマ区切り）" $
      parseSrc "f(1, 2 + 3, x)" `shouldBe` Right ([], [], Call "f" [Lit 1, Add (Lit 2) (Lit 3), Var "x"])
    it "呼び出し式は算術式の中で使える" $
      parseSrc "1 + f(2)" `shouldBe` Right ([], [], Add (Lit 1) (Call "f" [Lit 2]))
    it "呼び出し式は入れ子にできる" $
      parseSrc "f(g(1))" `shouldBe` Right ([], [], Call "f" [Call "g" [Lit 1]])
    it "return文（値付き）" $
      parseSrc "fn f() -> i64 {\nreturn 1;\n2\n}\n3"
        `shouldBe` Right
          ( [FnDecl "f" [] (TyInt W64) ([SReturn (Lit 1)], Lit 2)]
          , []
          , Lit 3
          )
    it "値の無いreturn文はエラー（ユニット型は採用しないため）" $
      parseSrc "fn f() -> i64 {\nreturn;\n1\n}\n2" `shouldSatisfy` isLeft
    it "return文に;が無ければエラー" $
      parseSrc "fn f() -> i64 {\nreturn 1\n2\n}\n3" `shouldSatisfy` isLeft

  describe "codegen" $ do
    it "プロローグにmainラベルを含む" $
      codegen [] (TyInt W64) [] `shouldContain` "main:"
    it "Push nをmovabsq+pushq命令に変換する" $
      codegen [] (TyInt W64) [Push 42] `shouldContain` "movabsq $42, %rax"
    it "IAdd W64をaddq命令に変換する" $
      codegen [] (TyInt W64) [IAdd W64] `shouldContain` "addq"
    it "ISub W64をsubq命令に変換する" $
      codegen [] (TyInt W64) [ISub W64] `shouldContain` "subq"
    it "IMul W64をimulq命令に変換する" $
      codegen [] (TyInt W64) [IMul W64] `shouldContain` "imulq"
    it "IDiv W64をidivq命令に変換する" $
      codegen [] (TyInt W64) [IDiv W64] `shouldContain` "idivq"
    it "INeg W64をnegq命令に変換する" $
      codegen [] (TyInt W64) [INeg W64] `shouldContain` "negq"
    it "IAdd W32をaddl命令に変換する" $
      codegen [] (TyInt W32) [IAdd W32] `shouldContain` "addl"
    it "ISub W32をsubl命令に変換する" $
      codegen [] (TyInt W32) [ISub W32] `shouldContain` "subl"
    it "IMul W32をimull命令に変換する" $
      codegen [] (TyInt W32) [IMul W32] `shouldContain` "imull"
    it "IDiv W32をidivl命令に変換する" $
      codegen [] (TyInt W32) [IDiv W32] `shouldContain` "idivl"
    it "INeg W32をnegl命令に変換する" $
      codegen [] (TyInt W32) [INeg W32] `shouldContain` "negl"
    it "IDivにゼロ除算チェックを含む" $
      codegen [] (TyInt W64) [IDiv W64] `shouldContain` ".Ldiv_zero_error"
    it "エピローグにprintf呼び出しを含む" $
      codegen [] (TyInt W64) [] `shouldContain` "call  printf"
    it "最終値がi64なら%ldフォーマットを使う" $
      codegen [] (TyInt W64) [] `shouldContain` "\"%ld\\n\""
    it "最終値がi32なら%dフォーマットを使う" $
      codegen [] (TyInt W32) [] `shouldContain` "\"%d\\n\""
    it "Load W64をmovq+pushqに変換する" $
      codegen [] (TyInt W64) [Load W64 (-8)] `shouldContain` "movq  -8(%rbp), %rax"
    it "Store W64をpopq+movqに変換する" $
      codegen [] (TyInt W64) [Store W64 (-8)] `shouldContain` "movq  %rax, -8(%rbp)"
    it "Load W32をmovslq+pushqに変換する（符号拡張ロード）" $
      codegen [] (TyInt W32) [Load W32 (-4)] `shouldContain` "movslq -4(%rbp), %rax"
    it "Store W32をpopq+movlに変換する（切り詰めストア）" $
      codegen [] (TyInt W32) [Store W32 (-4)] `shouldContain` "movl  %eax, -4(%rbp)"
    it "変数がある場合はsubqでスタックフレームを確保する（16バイト境界へ切り上げ）" $
      codegen [] (TyInt W64) [Store W64 (-8)] `shouldContain` "subq  $16, %rsp"
    it "変数が無ければsubqを出さない" $
      codegen [] (TyInt W64) [] `shouldNotContain` "subq"
    it "printf呼び出し前にスタックアライメントを揃える" $
      codegen [] (TyInt W64) [] `shouldContain` "andq  $-16, %rsp"
    it "エピローグはleave命令を使う" $
      codegen [] (TyInt W64) [] `shouldContain` "leave"
    it "ICmpEqをcmpq+sete命令に変換する" $
      codegen [] (TyInt W64) [ICmpEq] `shouldContain` "sete"
    it "ICmpNeをcmpq+setne命令に変換する" $
      codegen [] (TyInt W64) [ICmpNe] `shouldContain` "setne"
    it "ICmpLtをcmpq+setl命令に変換する" $
      codegen [] (TyInt W64) [ICmpLt] `shouldContain` "setl"
    it "ICmpLeをcmpq+setle命令に変換する" $
      codegen [] (TyInt W64) [ICmpLe] `shouldContain` "setle"
    it "ICmpGtをcmpq+setg命令に変換する" $
      codegen [] (TyInt W64) [ICmpGt] `shouldContain` "setg"
    it "ICmpGeをcmpq+setge命令に変換する" $
      codegen [] (TyInt W64) [ICmpGe] `shouldContain` "setge"
    it "INotをxorq命令に変換する" $
      codegen [] (TyInt W64) [INot] `shouldContain` "xorq"
    it "ISext32をcltq命令に変換する（to_i64/to_i32共通の符号拡張正規化）" $
      codegen [] (TyInt W64) [ISext32] `shouldContain` "cltq"
    it "JmpIfZeroはpop+testq+je命令に変換する" $ do
      let asm = codegen [] (TyInt W64) [JmpIfZero ".Lfoo"]
      asm `shouldContain` "testq %rax, %rax"
      asm `shouldContain` "je    .Lfoo"
    it "Jmpはjmp命令に変換する" $
      codegen [] (TyInt W64) [Jmp ".Lfoo"] `shouldContain` "jmp   .Lfoo"
    it "Labelはラベル定義行に変換する" $
      codegen [] (TyInt W64) [Label ".Lfoo"] `shouldContain` ".Lfoo:"
    it "最終値がboolなら分岐でtrue/false文字列を出力する" $ do
      let asm = codegen [] TBool []
      asm `shouldContain` "testq %rax, %rax"
      asm `shouldContain` "\"true\\n\""
      asm `shouldContain` "\"false\\n\""

  describe "意味論エラー（compile）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "let初期化式内の未宣言参照はエラー" $
      compileSource "let x: i64 = y;\nx" `shouldBe` Left "undeclared variable: y"
    it "let初期化式内の自己参照はエラー" $
      compileSource "let x: i64 = x + 1;\nx" `shouldBe` Left "undeclared variable: x"
    it "代入文右辺の未宣言参照はエラー" $
      compileSource "let x: i64 = 1;\nx = y + 1;\nx" `shouldBe` Left "undeclared variable: y"
    it "末尾式での未宣言参照はエラー" $
      compileSource "let x: i64 = 1;\ny" `shouldBe` Left "undeclared variable: y"
    it "再宣言はエラー" $
      compileSource "let x: i64 = 1;\nlet x: i64 = 2;\nx"
        `shouldBe` Left "variable already declared: x"
    it "型注釈がi32/i64/bool以外はエラー" $
      compileSource "let x: i16 = 1;\nx" `shouldBe` Left "unsupported type: i16"
    it "let初期化式でi32変数とi64変数を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nlet z: i64 = x + y;\nz"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "代入文右辺でi32変数とi64変数を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\ny = x + 1;\ny"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "末尾式でi32変数とi64変数を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nx + y"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "宣言した型と異なる型の変数を代入するとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nlet z: i32 = y;\nz"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "bool変数に整数リテラルを代入するとエラー" $
      compileSource "let x: bool = 1;\nx"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "整数変数にboolリテラルを代入するとエラー" $
      compileSource "let x: i32 = true;\nx"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "boolに対する算術演算はエラー" $
      compileSource "true + 1"
        `shouldBe` Left "type mismatch: expected bool, found arithmetic expression"
    it "整数変数への否定演算子の適用はエラー" $
      compileSource "let x: i32 = 1;\n!x"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "整数リテラルへの否定演算子の適用はエラー" $
      compileSource "!5"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "比較結果をintコンテキストで使うとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i32 = 2;\nlet z: i32 = x == y;\nz"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "末尾式での比較でi32とi64を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nx == y"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "let宣言の右辺での比較でi32とi64を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nlet z: bool = x == y;\nz"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "大小比較結果をintコンテキストで使うとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i32 = 2;\nlet z: i32 = x < y;\nz"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "末尾式での大小比較でi32とi64を混在させるとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i64 = 2;\nx < y"
        `shouldBe` Left "type mismatch: i32 and i64"
    it "bool同士の大小比較（<）はエラー" $
      compileSource "let x: bool = true;\nlet y: bool = false;\nx < y"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（<=）はエラー" $
      compileSource "true <= false"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（>）はエラー" $
      compileSource "true > false"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "bool同士の大小比較（>=）はエラー" $
      compileSource "true >= false"
        `shouldBe` Left "type mismatch: expected i32 or i64, found bool"
    it "to_i64にi64値を渡すとエラー（拡大変換の対象はi32のみ）" $
      compileSource "let x: i64 = 1;\nto_i64(x)"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "to_i32にi32値を渡すとエラー（縮小変換の対象はi64のみ）" $
      compileSource "let x: i32 = 1;\nto_i32(x)"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "to_i64にbool値を渡すとエラー" $
      compileSource "let x: bool = true;\nto_i64(x)"
        `shouldBe` Left "type mismatch: expected i32, found bool"
    it "to_i32にbool値を渡すとエラー" $
      compileSource "let x: bool = true;\nto_i32(x)"
        `shouldBe` Left "type mismatch: expected i64, found bool"
    it "to_i64の結果をi32コンテキストで使うとエラー" $
      compileSource "let x: i32 = 1;\nlet y: i32 = to_i64(x);\ny"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "to_i32の結果をi64コンテキストで使うとエラー" $
      compileSource "let x: i64 = 1;\nlet y: i64 = to_i32(x);\ny"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "to_i64の結果をboolコンテキストで使うとエラー" $
      compileSource "let x: i32 = 1;\nto_i64(x) == true"
        `shouldSatisfy` isLeft
    it "ブロックを抜けた後の内部宣言変数の参照はエラー" $
      compileSource "{\nlet x: i64 = 1;\n}\nx" `shouldBe` Left "undeclared variable: x"
    it "同一ブロック内での同名再宣言はエラー" $
      compileSource "{\nlet x: i64 = 1;\nlet x: i64 = 2;\n}\n1"
        `shouldBe` Left "variable already declared: x"
    it "外側と同名の変数をブロック内でletしてもエラーにならない（シャドーイング）" $
      compileSource "let x: i64 = 1;\n{\nlet x: i64 = 2;\n}\nx" `shouldSatisfy` isRight
    it "if文の条件式が整数リテラルはエラー" $
      compileSource "if 1 {\n}\n1" `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "if文の条件式が整数変数はエラー" $
      compileSource "let x: i32 = 1;\nif x {\n}\n1"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "if文の本体を抜けた後の内部宣言変数の参照はエラー" $
      compileSource "if true {\nlet x: i64 = 1;\n}\nx" `shouldBe` Left "undeclared variable: x"
    it "else if文の条件式が非bool式はエラー" $
      compileSource "if false {\n} else if 1 {\n}\n1"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "while文の条件式が整数リテラルはエラー" $
      compileSource "while 1 {\n}\n1"
        `shouldBe` Left "type mismatch: expected bool, found integer literal"
    it "while文の条件式が整数変数はエラー" $
      compileSource "let x: i32 = 1;\nwhile x {\n}\n1"
        `shouldBe` Left "type mismatch: expected bool, found i32"
    it "while文の本体を抜けた後の内部宣言変数の参照はエラー" $
      compileSource "while true {\nlet x: i64 = 1;\nbreak;\n}\nx" `shouldBe` Left "undeclared variable: x"
    it "break文をループ外で使うとエラー" $
      compileSource "break;\n1" `shouldBe` Left "break used outside loop"
    it "continue文をループ外で使うとエラー" $
      compileSource "continue;\n1" `shouldBe` Left "continue used outside loop"
    it "break文をif文の中（ループ外）で使うとエラー" $
      compileSource "if true {\nbreak;\n}\n1" `shouldBe` Left "break used outside loop"
    it "break文をwhileの外側のブロックで使うとエラー（ループを抜けた後は無効）" $
      compileSource "while true {\nbreak;\n}\n{\nbreak;\n}\n1" `shouldBe` Left "break used outside loop"

  describe "意味論エラー（fn定義・呼び出し）" $ do
    let compileSource src = tokenize src >>= parse >>= compile
    it "未定義関数の呼び出しはエラー" $
      compileSource "f()" `shouldBe` Left "undeclared function: f"
    it "引数個数が足りない呼び出しはエラー" $
      compileSource "fn f(a: i64, b: i64) -> i64 {\na + b\n}\nf(1)"
        `shouldBe` Left "wrong number of arguments for f: expected 2, found 1"
    it "引数個数が多すぎる呼び出しはエラー" $
      compileSource "fn f(a: i64) -> i64 {\na\n}\nf(1, 2)"
        `shouldBe` Left "wrong number of arguments for f: expected 1, found 2"
    it "引数の型が宣言と異なる呼び出しはエラー（暗黙変換は行わない）" $
      compileSource "fn f(a: i64) -> i64 {\na\n}\nlet x: i32 = 1;\nf(x)"
        `shouldBe` Left "type mismatch: expected i64, found i32"
    it "戻り値の型が期待と異なるとエラー" $
      compileSource "fn f() -> i64 {\n1\n}\nlet x: i32 = f();\nx"
        `shouldBe` Left "type mismatch: expected i32, found i64"
    it "関数名'main'は予約されておりエラー" $
      compileSource "fn main() -> i64 {\n1\n}\n2" `shouldBe` Left "function name 'main' is reserved"
    it "同名の関数を再定義するとエラー" $
      compileSource "fn f() -> i64 {\n1\n}\nfn f() -> i64 {\n2\n}\n3"
        `shouldBe` Left "function already declared: f"
    it "引数が7個を超える関数定義はエラー（レジスタ渡しの上限）" $
      compileSource
        "fn f(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, g: i64) -> i64 {\na\n}\nf(1,2,3,4,5,6,7)"
        `shouldBe` Left "too many parameters (max 6): f"
    it "関数外でのreturnはエラー" $
      compileSource "return 1;\n2" `shouldBe` Left "return used outside function"
    it "宣言順に依存しない相互再帰は成功する（二パスコンパイルの確認）" $
      compileSource
        "fn is_even(n: i64) -> bool {\nif n == 0 {\nreturn true;\n}\nis_odd(n - 1)\n}\nfn is_odd(n: i64) -> bool {\nif n == 0 {\nreturn false;\n}\nis_even(n - 1)\n}\nis_even(4)"
        `shouldSatisfy` isRight

  describe "run（VM）" $ do
    it "Load/Storeで変数の値を保持する" $
      run [Push 1, Store W64 (-8), Load W64 (-8), Push 2, IAdd W64] `shouldBe` Right 3
    it "IAdd W32はi32範囲でラップアラウンドする" $
      run [Push 2147483647, Push 1, IAdd W32] `shouldBe` Right (-2147483648)
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
    it "ISext32はi32範囲内の値をそのまま保持する（to_i64での値保存の確認）" $
      run [Push 42, ISext32] `shouldBe` Right 42
    it "ISext32はi32範囲外の値をラップアラウンドさせる（to_i32での縮小変換の確認）" $
      run [Push 4294967296, ISext32] `shouldBe` Right 0
    it "ISext32はi32範囲外の値を符号拡張してラップアラウンドさせる" $
      run [Push 2147483648, ISext32] `shouldBe` Right (-2147483648)

  describe "compile + codegen + gcc（結合テスト）" $ do
    it "リテラルを評価する" $ do
      result <- compileAndRun (Lit 7)
      result `shouldBe` "7"
    it "加算を評価する" $ do
      result <- compileAndRun (Add (Lit 3) (Lit 4))
      result `shouldBe` "7"
    it "減算を評価する" $ do
      result <- compileAndRun (Sub (Lit 5) (Lit 3))
      result `shouldBe` "2"
    it "乗算を評価する" $ do
      result <- compileAndRun (Mul (Lit 2) (Lit 6))
      result `shouldBe` "12"
    it "除算を評価する" $ do
      result <- compileAndRun (Div (Lit 6) (Lit 3))
      result `shouldBe` "2"
    it "単項マイナスを評価する" $ do
      result <- compileAndRun (Neg (Lit 5))
      result `shouldBe` "-5"
    it "複合式を評価する: 1 + 2 * (3 - 4) = -1" $ do
      result <- compileAndRun (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4))))
      result `shouldBe` "-1"
    it "boolリテラル（true）を評価する" $ do
      result <- compileAndRun (BoolLit True)
      result `shouldBe` "true"
    it "boolリテラル（false）を評価する" $ do
      result <- compileAndRun (BoolLit False)
      result `shouldBe` "false"
    it "等価演算子を評価する（等しい場合）" $ do
      result <- compileAndRun (Eq (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "等価演算子を評価する（等しくない場合）" $ do
      result <- compileAndRun (Eq (Lit 3) (Lit 4))
      result `shouldBe` "false"
    it "非等価演算子を評価する" $ do
      result <- compileAndRun (Neq (Lit 3) (Lit 4))
      result `shouldBe` "true"
    it "小なり演算子を評価する（true）" $ do
      result <- compileAndRun (Lt (Lit 3) (Lit 4))
      result `shouldBe` "true"
    it "小なり演算子を評価する（false）" $ do
      result <- compileAndRun (Lt (Lit 4) (Lit 3))
      result `shouldBe` "false"
    it "以下演算子を評価する（等しい場合はtrue）" $ do
      result <- compileAndRun (Le (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "大なり演算子を評価する（true）" $ do
      result <- compileAndRun (Gt (Lit 4) (Lit 3))
      result `shouldBe` "true"
    it "大なり演算子を評価する（false）" $ do
      result <- compileAndRun (Gt (Lit 3) (Lit 4))
      result `shouldBe` "false"
    it "以上演算子を評価する（等しい場合はtrue）" $ do
      result <- compileAndRun (Ge (Lit 3) (Lit 3))
      result `shouldBe` "true"
    it "否定演算子を評価する" $ do
      result <- compileAndRun (Not (BoolLit False))
      result `shouldBe` "true"
    it "to_i32(リテラル)を評価する" $ do
      result <- compileAndRun (ToI32 (Lit 64))
      result `shouldBe` "64"

  describe "compile + codegen + gcc（変数を含むソースの結合テスト）" $ do
    it "複数の宣言・代入・末尾式を評価する" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\nlet y: i64 = 2;\nx = x + y;\nx"
      result `shouldBe` "3"
    it "変数1個（奇数オフセット）でもアライメントが崩れない" $ do
      result <- compileSourceAndRun "let x: i64 = 5;\nx + 1"
      result `shouldBe` "6"
    it "i32変数の宣言・演算・出力を評価する" $ do
      result <- compileSourceAndRun "let x: i32 = 1;\nlet y: i32 = 2;\nx = x + y;\nx"
      result `shouldBe` "3"
    it "i32のオーバーフローは32bit範囲でラップアラウンドする" $ do
      result <- compileSourceAndRun "let x: i32 = 2147483647;\nx + 1"
      result `shouldBe` "-2147483648"
    it "i32の境界値（INT32_MIN）を正しく扱う（movabsq修正の確認）" $ do
      result <- compileSourceAndRun "let x: i32 = -2147483648;\nx"
      result `shouldBe` "-2147483648"
    it "i32とi64が交互に宣言されてもタイトパッキングで正しく動作する（式自体は単一型を維持）" $ do
      result <- compileSourceAndRun "let a: i32 = 1;\nlet b: i64 = 2;\nlet c: i32 = 3;\na + c"
      result `shouldBe` "4"
    it "i64の末尾式は%ldで正しく出力される（64bit値の潜在バグ修正の確認）" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\nlet y: i64 = 2;\ny"
      result `shouldBe` "2"
    it "bool変数の宣言・末尾式出力を評価する" $ do
      result <- compileSourceAndRun "let flag: bool = true;\nflag"
      result `shouldBe` "true"
    it "bool変数への代入を評価する" $ do
      result <- compileSourceAndRun "let flag: bool = true;\nflag = false;\nflag"
      result `shouldBe` "false"
    it "i32変数同士の等価比較を評価する（true）" $ do
      result <- compileSourceAndRun "let a: i32 = 3;\nlet b: i32 = 3;\na == b"
      result `shouldBe` "true"
    it "i32変数同士の等価比較を評価する（false）" $ do
      result <- compileSourceAndRun "let a: i32 = 3;\nlet b: i32 = 4;\na == b"
      result `shouldBe` "false"
    it "i32変数同士の非等価比較を評価する" $ do
      result <- compileSourceAndRun "let a: i32 = 3;\nlet b: i32 = 4;\na != b"
      result `shouldBe` "true"
    it "i64変数同士の等価比較を評価する" $ do
      result <- compileSourceAndRun "let x: i64 = 100;\nlet y: i64 = 100;\nx == y"
      result `shouldBe` "true"
    it "否定演算子と括弧を組み合わせて評価する" $ do
      result <- compileSourceAndRun "!(1 == 2)"
      result `shouldBe` "true"
    it "算術式の結果同士の比較を評価する" $ do
      result <- compileSourceAndRun "let x: i32 = 1;\nlet y: i32 = 2;\n(x + 1) == y"
      result `shouldBe` "true"
    it "i32変数同士の小なり比較を評価する" $ do
      result <- compileSourceAndRun "let a: i32 = 3;\nlet b: i32 = 4;\na < b"
      result `shouldBe` "true"
    it "i32変数同士の以下比較を評価する（等しい場合）" $ do
      result <- compileSourceAndRun "let a: i32 = 3;\nlet b: i32 = 3;\na <= b"
      result `shouldBe` "true"
    it "i64変数同士の大なり比較を評価する" $ do
      result <- compileSourceAndRun "let x: i64 = 100;\nlet y: i64 = 50;\nx > y"
      result `shouldBe` "true"
    it "i64変数同士の以上比較を評価する（等しい場合）" $ do
      result <- compileSourceAndRun "let x: i64 = 100;\nlet y: i64 = 100;\nx >= y"
      result `shouldBe` "true"
    it "否定演算子と小なり比較を組み合わせて評価する" $ do
      result <- compileSourceAndRun "!(1 < 2)"
      result `shouldBe` "false"
    it "to_i64はi32の値をそのままi64として保持する（拡大変換は値を保存する）" $ do
      result <- compileSourceAndRun "let x: i32 = 2147483647;\nlet y: i64 = to_i64(x);\ny"
      result `shouldBe` "2147483647"
    it "to_i32はi32範囲外のi64値をラップアラウンドさせる（縮小変換）" $ do
      result <- compileSourceAndRun "let x: i64 = 4294967296;\nlet y: i32 = to_i32(x) + 5;\ny"
      result `shouldBe` "5"
    it "to_i64/to_i32は括弧なしの単項演算子構文でも評価できる" $ do
      result <- compileSourceAndRun "let x: i64 = 4294967296;\nlet y: i32 = to_i32 x + 5;\ny"
      result `shouldBe` "5"
    it "to_i32とto_i64のラウンドトリップ（i32->i64->i32）は値を保存する" $ do
      result <- compileSourceAndRun "let x: i32 = 42;\nto_i32(to_i64(x))"
      result `shouldBe` "42"
    it "ブロック内からの代入は外側の変数へ反映される" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\n{\nx = 2;\n}\nx"
      result `shouldBe` "2"
    it "シャドーイングされた変数はブロックを抜けると外側の値に戻る" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\n{\nlet x: i64 = 99;\n}\nx"
      result `shouldBe` "1"
    it "兄弟ブロックはそれぞれ独立にローカル変数を持てる（同名変数の再利用を含む）" $ do
      result <- compileSourceAndRun
        "let sum: i64 = 0;\n{\nlet y: i64 = 1;\nsum = sum + y;\n}\n{\nlet y: i64 = 2;\nsum = sum + y;\n}\nsum"
      result `shouldBe` "3"
    it "空ブロックは合法で何もしない" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\n{}\nx"
      result `shouldBe` "1"

  describe "compile + codegen + gcc（if文の結合テスト）" $ do
    it "条件が真ならif本体を実行する" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\nif true {\nx = 1;\n}\nx"
      result `shouldBe` "1"
    it "条件が偽ならif本体を実行しない（elseなし）" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\nif false {\nx = 1;\n}\nx"
      result `shouldBe` "0"
    it "条件が偽ならelse本体を実行する" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\nif false {\nx = 1;\n} else {\nx = 2;\n}\nx"
      result `shouldBe` "2"
    it "条件が真ならelseは実行されない" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\nif true {\nx = 1;\n} else {\nx = 2;\n}\nx"
      result `shouldBe` "1"
    it "比較演算子を条件式に使える" $ do
      result <- compileSourceAndRun "let x: i32 = 3;\nlet y: i32 = 0;\nif x == 3 {\ny = 1;\n} else {\ny = 2;\n}\ny"
      result `shouldBe` "1"
    it "大小比較演算子を条件式に使える" $ do
      result <- compileSourceAndRun "let x: i32 = 3;\nlet y: i32 = 0;\nif x < 5 {\ny = 1;\n} else {\ny = 2;\n}\ny"
      result `shouldBe` "1"
    it "else ifで最初に真になった分岐だけを実行する" $ do
      result <- compileSourceAndRun
        "let x: i64 = 2;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n} else if x == 2 {\ny = 3;\n} else {\ny = 4;\n}\ny"
      result `shouldBe` "2"
    it "else ifが全て偽ならelse本体を実行する" $ do
      result <- compileSourceAndRun
        "let x: i64 = 9;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n} else {\ny = 3;\n}\ny"
      result `shouldBe` "3"
    it "else ifが全て偽でelseも無ければ何も実行しない" $ do
      result <- compileSourceAndRun
        "let x: i64 = 9;\nlet y: i64 = 0;\nif x == 1 {\ny = 1;\n} else if x == 2 {\ny = 2;\n}\ny"
      result `shouldBe` "0"
    it "ifをネストできる" $ do
      result <- compileSourceAndRun
        "let x: i64 = 1;\nif x == 1 {\nif x == 1 {\nx = 42;\n}\n}\nx"
      result `shouldBe` "42"
    it "ブロック内にifをネストできる" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\n{\nif x == 1 {\nx = 42;\n}\n}\nx"
      result `shouldBe` "42"
    it "then節とelse節はそれぞれ独立したローカル変数を持てる（同名変数の再利用を含む）" $ do
      result <- compileSourceAndRun
        "let sum: i64 = 0;\nif true {\nlet y: i64 = 1;\nsum = sum + y;\n} else {\nlet y: i64 = 99;\nsum = sum + y;\n}\nsum"
      result `shouldBe` "1"
    it "if本体を抜けるとその中で宣言した変数は不可視になる（シャドーイングも含めた回帰確認）" $ do
      result <- compileSourceAndRun
        "let x: i64 = 1;\nif true {\nlet x: i64 = 99;\nx = 2;\n}\nx"
      result `shouldBe` "1"
    it "複数のifが連続しても互いに独立して動作する" $ do
      result <- compileSourceAndRun
        "let a: i64 = 0;\nlet b: i64 = 0;\nif true {\na = 1;\n} else {\na = 2;\n}\nif false {\nb = 1;\n} else {\nb = 2;\n}\na + b"
      result `shouldBe` "3"

  describe "compile + codegen + gcc（while文の結合テスト）" $ do
    it "基本的なカウントダウンループを実行する" $ do
      result <- compileSourceAndRun
        "let x: i64 = 3;\nlet sum: i64 = 0;\nwhile x != 0 {\nsum = sum + x;\nx = x - 1;\n}\nsum"
      result `shouldBe` "6"
    it "条件が最初から偽なら本体を一度も実行しない" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\nwhile x != 0 {\nx = 1;\n}\nx"
      result `shouldBe` "0"
    it "breakでループを早期終了する" $ do
      result <- compileSourceAndRun
        "let x: i64 = 0;\nwhile true {\nx = x + 1;\nif x == 3 {\nbreak;\n}\n}\nx"
      result `shouldBe` "3"
    it "continueで本体の残りをスキップし条件を再評価する" $ do
      result <- compileSourceAndRun
        "let x: i64 = 0;\nlet sum: i64 = 0;\nwhile x != 5 {\nx = x + 1;\nif x == 3 {\ncontinue;\n}\nsum = sum + x;\n}\nsum"
      result `shouldBe` "12"
    it "whileをネストできる（内側のbreakは外側に影響しない）" $ do
      result <- compileSourceAndRun
        "let i: i64 = 0;\nlet count: i64 = 0;\nwhile i != 3 {\nlet j: i64 = 0;\nwhile j != 5 {\nif j == 2 {\nbreak;\n}\ncount = count + 1;\nj = j + 1;\n}\ni = i + 1;\n}\ncount"
      result `shouldBe` "6"
    it "whileのネストで内側のcontinueは外側ループに影響しない" $ do
      result <- compileSourceAndRun
        "let i: i64 = 0;\nlet count: i64 = 0;\nwhile i != 2 {\nlet j: i64 = 0;\nwhile j != 3 {\nj = j + 1;\nif j == 2 {\ncontinue;\n}\ncount = count + 1;\n}\ni = i + 1;\n}\ncount"
      result `shouldBe` "4"
    it "while本体を抜けるとその中で宣言した変数は不可視になる（毎イテレーション巻き戻る）" $ do
      result <- compileSourceAndRun
        "let x: i64 = 0;\nlet i: i64 = 0;\nwhile i != 3 {\nlet y: i64 = 100;\ny = y + 1;\ni = i + 1;\n}\nx"
      result `shouldBe` "0"
    it "ブロック内にwhileをネストできる" $ do
      result <- compileSourceAndRun "let x: i64 = 0;\n{\nwhile x != 3 {\nx = x + 1;\n}\n}\nx"
      result `shouldBe` "3"
    it "複数のwhileが連続しても互いに独立して動作する（ラベルの一意性の間接的な確認）" $ do
      result <- compileSourceAndRun
        "let a: i64 = 0;\nwhile a != 2 {\na = a + 1;\n}\nlet b: i64 = 0;\nwhile b != 3 {\nb = b + 1;\n}\na + b"
      result `shouldBe` "5"
    it "小なり演算子をwhileの条件式に使える（sample/src008.sl相当）" $ do
      result <- compileSourceAndRun
        "let a: i64 = 0;\nlet sum: i64 = 0;\nwhile a < 10 {\nsum = sum + a;\na = a + 1;\n}\nsum"
      result `shouldBe` "45"

  describe "compile + codegen + gcc（fn定義・呼び出しの結合テスト）" $ do
    it "単純な関数呼び出しを評価する" $ do
      result <- compileSourceAndRun "fn add(a: i64, b: i64) -> i64 {\na + b\n}\nlet x: i64 = add(1, 2);\nx"
      result `shouldBe` "3"
    it "引数の型がi32の関数を評価する" $ do
      result <- compileSourceAndRun "fn add(a: i32, b: i32) -> i32 {\na + b\n}\nadd(1, 2)"
      result `shouldBe` "3"
    it "戻り値がboolの関数を評価する" $ do
      result <- compileSourceAndRun "fn isPositive(x: i64) -> bool {\nx > 0\n}\nisPositive(5)"
      result `shouldBe` "true"
    it "早期returnを評価する" $ do
      result <- compileSourceAndRun
        "fn abs(x: i64) -> i64 {\nif x < 0 {\nreturn 0 - x;\n}\nx\n}\nabs(0 - 5)"
      result `shouldBe` "5"
    it "自己再帰（階乗）を評価する" $ do
      result <- compileSourceAndRun
        "fn fact(n: i64) -> i64 {\nif n <= 1 {\nreturn 1;\n}\nn * fact(n - 1)\n}\nfact(10)"
      result `shouldBe` "3628800"
    it "宣言順に依存しない相互再帰を評価する" $ do
      result <- compileSourceAndRun
        "fn is_even(n: i64) -> bool {\nif n == 0 {\nreturn true;\n}\nis_odd(n - 1)\n}\nfn is_odd(n: i64) -> bool {\nif n == 0 {\nreturn false;\n}\nis_even(n - 1)\n}\nis_even(10)"
      result `shouldBe` "true"
    it "6個の引数（レジスタ渡しの上限）を持つ関数を評価する" $ do
      result <- compileSourceAndRun
        "fn sum6(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64) -> i64 {\na + b + c + d + e + f\n}\nsum6(1, 2, 3, 4, 5, 6)"
      result `shouldBe` "21"
    it "呼び出しは他の呼び出しの引数として入れ子にできる" $ do
      result <- compileSourceAndRun
        "fn inc(x: i64) -> i64 {\nx + 1\n}\ninc(inc(inc(0)))"
      result `shouldBe` "3"
    it "式の途中（奇数深さ）でのユーザー関数呼び出しでもスタックアライメントが崩れない" $ do
      result <- compileSourceAndRun "fn foo(x: i64) -> i64 {\nx * 2\n}\n1 + 2 + foo(3)"
      result `shouldBe` "9"
    it "fn定義は暗黙main本体の文と自由に混在できる（実行結果の確認）" $ do
      result <- compileSourceAndRun
        "let a: i64 = 1;\nfn double(x: i64) -> i64 {\nx * 2\n}\nlet b: i64 = double(a);\nfn triple(x: i64) -> i64 {\nx * 3\n}\ntriple(b)"
      result `shouldBe` "6"

  describe "ゼロ除算の実行時エラー" $ do
    it "変数なしのゼロ除算はエラーメッセージを出力して非ゼロ終了する" $ do
      (code, out) <- compileSourceAndRunExit "1 / 0"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "変数がある状態でのゼロ除算でもアライメントが崩れない" $ do
      (code, out) <- compileSourceAndRunExit "let x: i64 = 5;\nx / 0"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess
    it "i32変数のゼロ除算もエラーメッセージを出力して非ゼロ終了する" $ do
      (code, out) <- compileSourceAndRunExit "let x: i32 = 5;\nx / 0"
      out `shouldBe` "division by zero"
      code `shouldNotBe` ExitSuccess

compileAndRun :: Expr -> IO String
compileAndRun expr =
  withSystemTempDirectory "hs006" $ \tmpDir -> do
    let asmPath = tmpDir </> "out.s"
        binPath = tmpDir </> "out"
    case compile ([], [], expr) of
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
