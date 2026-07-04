module Main where

import Compiler (Expr (..), Instr (..), Stmt (..), Token (..), compile, parse, run, tokenize)
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
    it "let宣言" $
      parse [TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Right ([SLet "x" (Lit 1)], Var "x")
    it "代入文" $
      parse
        [ TLet, TIdent "x", TColon, TIdent "i64", TAssign, TInt 1, TSemicolon
        , TIdent "x", TAssign, TInt 2, TSemicolon
        , TIdent "x"
        ]
        `shouldBe` Right ([SLet "x" (Lit 1), SAssign "x" (Lit 2)], Var "x")
    it "let宣言でコロンが無ければエラー" $
      parse [TLet, TIdent "x", TAssign, TInt 1, TSemicolon, TIdent "x"] `shouldSatisfy` isLeft
    it "let宣言で型がi64以外ならエラー" $
      parse [TLet, TIdent "x", TColon, TIdent "i32", TAssign, TInt 1, TSemicolon, TIdent "x"]
        `shouldBe` Left "unsupported type: i32"
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

  describe "codegen" $ do
    it "プロローグにmainラベルを含む" $
      codegen [] `shouldContain` "main:"
    it "Push nをpushq命令に変換する" $
      codegen [Push 42] `shouldContain` "pushq $42"
    it "IAddをaddq命令に変換する" $
      codegen [IAdd] `shouldContain` "addq"
    it "ISubをsubq命令に変換する" $
      codegen [ISub] `shouldContain` "subq"
    it "IMulをimulq命令に変換する" $
      codegen [IMul] `shouldContain` "imulq"
    it "IDivをidivq命令に変換する" $
      codegen [IDiv] `shouldContain` "idivq"
    it "INegをnegq命令に変換する" $
      codegen [INeg] `shouldContain` "negq"
    it "IDivにゼロ除算チェックを含む" $
      codegen [IDiv] `shouldContain` ".Ldiv_zero_error"
    it "エピローグにprintf呼び出しを含む" $
      codegen [] `shouldContain` "call  printf"
    it "Loadをmovq+pushqに変換する" $
      codegen [Load (-8)] `shouldContain` "movq  -8(%rbp), %rax"
    it "Storeをpopq+movqに変換する" $
      codegen [Store (-8)] `shouldContain` "movq  %rax, -8(%rbp)"
    it "変数がある場合はsubqでスタックフレームを確保する" $
      codegen [Store (-8)] `shouldContain` "subq  $8, %rsp"
    it "変数が無ければsubqを出さない" $
      codegen [] `shouldNotContain` "subq"
    it "printf呼び出し前にスタックアライメントを揃える" $
      codegen [] `shouldContain` "andq  $-16, %rsp"
    it "エピローグはleave命令を使う" $
      codegen [] `shouldContain` "leave"

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
    it "型注釈がi64以外はエラー" $
      compileSource "let x: i32 = 1;\nx" `shouldBe` Left "unsupported type: i32"

  describe "run（VM）" $ do
    it "Load/Storeで変数の値を保持する" $
      run [Push 1, Store (-8), Load (-8), Push 2, IAdd] `shouldBe` Right 3

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

  describe "compile + codegen + gcc（変数を含むソースの結合テスト）" $ do
    it "複数の宣言・代入・末尾式を評価する" $ do
      result <- compileSourceAndRun "let x: i64 = 1;\nlet y: i64 = 2;\nx = x + y;\nx"
      result `shouldBe` "3"
    it "変数1個（奇数オフセット）でもアライメントが崩れない" $ do
      result <- compileSourceAndRun "let x: i64 = 5;\nx + 1"
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

compileAndRun :: Expr -> IO String
compileAndRun expr =
  withSystemTempDirectory "hs006" $ \tmpDir -> do
    let asmPath = tmpDir </> "out.s"
        binPath = tmpDir </> "out"
    case compile ([], expr) of
      Left err -> error ("compile failed: " ++ err)
      Right instrs -> do
        writeFile asmPath (codegen instrs)
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
      Right instrs -> do
        writeFile asmPath (codegen instrs)
        _ <- readProcess "gcc" [asmPath, "-o", binPath] ""
        runBinary binPath

compileSourceAndRun :: String -> IO String
compileSourceAndRun = buildAndRun (\binPath -> takeWhile (/= '\n') <$> readProcess binPath [] "")

-- 非ゼロ終了するプログラム用: readProcess は非ゼロ終了時に例外を投げるため使えない。
compileSourceAndRunExit :: String -> IO (ExitCode, String)
compileSourceAndRunExit = buildAndRun $ \binPath -> do
  (code, out, _err) <- readProcessWithExitCode binPath [] ""
  return (code, takeWhile (/= '\n') out)
