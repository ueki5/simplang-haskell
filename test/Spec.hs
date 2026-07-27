module Main where

import Compiler (Expr (..), Instr (..), Stmt (..), Token (..), Type (..), Width (..), compile, parse, run, tokenize)
import Data.Either (isLeft)
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
    it "!演算子を変換する" $
      tokenize "!" `shouldBe` Right [TBang]
    it "=単体は引き続きTAssignになる（==との判別の回帰確認）" $
      tokenize "=" `shouldBe` Right [TAssign]
    it "true/false予約語を変換する" $
      tokenize "true false" `shouldBe` Right [TTrue, TFalse]
    it "true/falseで始まる識別子はTIdentのまま" $
      tokenize "truest falsely" `shouldBe` Right [TIdent "truest", TIdent "falsely"]

  describe "parse" $ do
    it "整数リテラル" $
      parse [TInt 5] `shouldBe` Right ([], Lit 5)
    it "加算" $
      parse [TInt 1, TPlus, TInt 2] `shouldBe` Right ([], Add (Lit 1) (Lit 2))
    it "減算" $
      parse [TInt 3, TMinus, TInt 1] `shouldBe` Right ([], Sub (Lit 3) (Lit 1))
    it "乗算が加算より優先される" $
      parse [TInt 1, TPlus, TInt 2, TStar, TInt 3]
        `shouldBe` Right ([], Add (Lit 1) (Mul (Lit 2) (Lit 3)))
    it "括弧で優先度を変える" $
      parse [TLParen, TInt 1, TPlus, TInt 2, TRParen, TStar, TInt 3]
        `shouldBe` Right ([], Mul (Add (Lit 1) (Lit 2)) (Lit 3))
    it "単項マイナス" $
      parse [TMinus, TInt 5] `shouldBe` Right ([], Neg (Lit 5))
    it "空入力はエラー" $
      parse [] `shouldBe` Left "unexpected end of input"
    it "変数参照" $
      parse [TIdent "x"] `shouldBe` Right ([], Var "x")
    it "let宣言（i64）" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([SLet "x" (TyInt W64) (Lit 1)], Var "x")
    it "let宣言（i32）" $
      parse [TLet, TIdent "x", TColon, TIdent "i32", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([SLet "x" (TyInt W32) (Lit 1)], Var "x")
    it "let宣言（bool）" $
      parse [TLet, TIdent "x", TColon, TIdent "bool", TAssign, TTrue, TSemicolon, TIdent "x"]
        `shouldBe` Right ([SLet "x" TBool (BoolLit True)], Var "x")
    it "代入文" $
      parse
        [ TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TIdent "x", TAssign, TInt 2, TSemicolon
        , TIdent "x"
        ]
        `shouldBe` Right ([SLet "x" (TyInt W64) (Lit 1), SAssign "x" (Lit 2)], Var "x")
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
      parse [TTrue] `shouldBe` Right ([], BoolLit True)
    it "boolリテラル（false）" $
      parse [TFalse] `shouldBe` Right ([], BoolLit False)
    it "等価演算子" $
      parse [TInt 1, TEq, TInt 2] `shouldBe` Right ([], Eq (Lit 1) (Lit 2))
    it "非等価演算子" $
      parse [TInt 1, TNeq, TInt 2] `shouldBe` Right ([], Neq (Lit 1) (Lit 2))
    it "否定演算子" $
      parse [TBang, TTrue] `shouldBe` Right ([], Not (BoolLit True))
    it "等価演算子は加減算より優先順位が低い" $
      parse [TInt 1, TPlus, TInt 2, TEq, TInt 3]
        `shouldBe` Right ([], Eq (Add (Lit 1) (Lit 2)) (Lit 3))
    it "否定演算子は等価演算子より強く結合する" $
      parse [TBang, TTrue, TEq, TFalse]
        `shouldBe` Right ([], Eq (Not (BoolLit True)) (BoolLit False))
    it "括弧内の等価演算子をパースできる" $
      parse [TLParen, TInt 1, TEq, TInt 1, TRParen]
        `shouldBe` Right ([], Eq (Lit 1) (Lit 1))

  describe "codegen" $ do
    it "プロローグにmainラベルを含む" $
      codegen (TyInt W64) [] `shouldContain` "main:"
    it "Push nをmovabsq+pushq命令に変換する" $
      codegen (TyInt W64) [Push 42] `shouldContain` "movabsq $42, %rax"
    it "IAdd W64をaddq命令に変換する" $
      codegen (TyInt W64) [IAdd W64] `shouldContain` "addq"
    it "ISub W64をsubq命令に変換する" $
      codegen (TyInt W64) [ISub W64] `shouldContain` "subq"
    it "IMul W64をimulq命令に変換する" $
      codegen (TyInt W64) [IMul W64] `shouldContain` "imulq"
    it "IDiv W64をidivq命令に変換する" $
      codegen (TyInt W64) [IDiv W64] `shouldContain` "idivq"
    it "INeg W64をnegq命令に変換する" $
      codegen (TyInt W64) [INeg W64] `shouldContain` "negq"
    it "IAdd W32をaddl命令に変換する" $
      codegen (TyInt W32) [IAdd W32] `shouldContain` "addl"
    it "ISub W32をsubl命令に変換する" $
      codegen (TyInt W32) [ISub W32] `shouldContain` "subl"
    it "IMul W32をimull命令に変換する" $
      codegen (TyInt W32) [IMul W32] `shouldContain` "imull"
    it "IDiv W32をidivl命令に変換する" $
      codegen (TyInt W32) [IDiv W32] `shouldContain` "idivl"
    it "INeg W32をnegl命令に変換する" $
      codegen (TyInt W32) [INeg W32] `shouldContain` "negl"
    it "IDivにゼロ除算チェックを含む" $
      codegen (TyInt W64) [IDiv W64] `shouldContain` ".Ldiv_zero_error"
    it "エピローグにprintf呼び出しを含む" $
      codegen (TyInt W64) [] `shouldContain` "call  printf"
    it "最終値がi64なら%ldフォーマットを使う" $
      codegen (TyInt W64) [] `shouldContain` "\"%ld\\n\""
    it "最終値がi32なら%dフォーマットを使う" $
      codegen (TyInt W32) [] `shouldContain` "\"%d\\n\""
    it "Load W64をmovq+pushqに変換する" $
      codegen (TyInt W64) [Load W64 (-8)] `shouldContain` "movq  -8(%rbp), %rax"
    it "Store W64をpopq+movqに変換する" $
      codegen (TyInt W64) [Store W64 (-8)] `shouldContain` "movq  %rax, -8(%rbp)"
    it "Load W32をmovslq+pushqに変換する（符号拡張ロード）" $
      codegen (TyInt W32) [Load W32 (-4)] `shouldContain` "movslq -4(%rbp), %rax"
    it "Store W32をpopq+movlに変換する（切り詰めストア）" $
      codegen (TyInt W32) [Store W32 (-4)] `shouldContain` "movl  %eax, -4(%rbp)"
    it "変数がある場合はsubqでスタックフレームを確保する" $
      codegen (TyInt W64) [Store W64 (-8)] `shouldContain` "subq  $8, %rsp"
    it "変数が無ければsubqを出さない" $
      codegen (TyInt W64) [] `shouldNotContain` "subq"
    it "printf呼び出し前にスタックアライメントを揃える" $
      codegen (TyInt W64) [] `shouldContain` "andq  $-16, %rsp"
    it "エピローグはleave命令を使う" $
      codegen (TyInt W64) [] `shouldContain` "leave"
    it "ICmpEqをcmpq+sete命令に変換する" $
      codegen (TyInt W64) [ICmpEq] `shouldContain` "sete"
    it "ICmpNeをcmpq+setne命令に変換する" $
      codegen (TyInt W64) [ICmpNe] `shouldContain` "setne"
    it "INotをxorq命令に変換する" $
      codegen (TyInt W64) [INot] `shouldContain` "xorq"
    it "最終値がboolなら分岐でtrue/false文字列を出力する" $ do
      let asm = codegen TBool []
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
    it "INotは0を1に反転する" $
      run [Push 0, INot] `shouldBe` Right 1
    it "INotは1を0に反転する" $
      run [Push 1, INot] `shouldBe` Right 0

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
    it "否定演算子を評価する" $ do
      result <- compileAndRun (Not (BoolLit False))
      result `shouldBe` "true"

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
    case compile ([], expr) of
      Left err -> error ("compile failed: " ++ err)
      Right (finalType, instrs) -> do
        writeFile asmPath (codegen finalType instrs)
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
      Right (finalType, instrs) -> do
        writeFile asmPath (codegen finalType instrs)
        _ <- readProcess "gcc" [asmPath, "-o", binPath] ""
        runBinary binPath

compileSourceAndRun :: String -> IO String
compileSourceAndRun = buildAndRun (\binPath -> takeWhile (/= '\n') <$> readProcess binPath [] "")

-- 非ゼロ終了するプログラム用: readProcess は非ゼロ終了時に例外を投げるため使えない。
compileSourceAndRunExit :: String -> IO (ExitCode, String)
compileSourceAndRunExit = buildAndRun $ \binPath -> do
  (code, out, _err) <- readProcessWithExitCode binPath [] ""
  return (code, takeWhile (/= '\n') out)
