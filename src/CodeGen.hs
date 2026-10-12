module CodeGen (codegen) where

import Compiler (Instr (..))
import Data.List (mapAccumL)
import Parser (Signedness (..), Type (..), Width (..))

-- -- Debug Printサンプル（pTraceShow: 純粋関数内, pTraceShowM: モナド内）
-- import Debug.Pretty.Simple (pTraceShow, pTraceShowM)

-- -- プリティ印刷が不要な場合
-- -- trace: 純粋関数内(既存の第一引数を表示、第二引数を返却)
-- -- print/putStrLn: IOモナド内
-- import Debug.Trace (trace)

-- main以外の各fn（(名前, 命令列)）＋main（戻り値の型, 命令列）からアセンブリ全体を組み立てる。
-- .text/.globl main はファイル全体で一度だけ、各fnラベルはその後に、mainはmain:として
-- 最後に並べる（GASのラベル解決は出現順に依存しないため順序自体に意味はない）
codegen :: [(String, [Instr])] -> Type -> [Instr] -> String
codegen fns finalType mainInstrs =
  unlines $
    ["    .section .text", "    .globl main"]
      ++ concatMap genFn fns
      ++ genMain
      ++ commonTail
 where
  genFn (name, instrs) =
    (name ++ ":")
      : prologueBody (frameSize instrs)
      ++ genBody instrs
      ++ epilogueRet
  genMain =
    "main:"
      : prologueBody (frameSize mainInstrs)
      ++ genBody mainInstrs
      ++ epilogue finalType

-- ローカル変数・仮引数のスピル用に確保するスタックフレームのサイズ（バイト、16バイト境界に切り上げ）。
-- Load/Store/StoreArg が保持する %rbp 相対オフセットの絶対値の最大が最低限必要な確保量になる。
-- 16バイトへの切り上げは、プロローグ直後を「操作スタックの深さ0＝16バイト境界」という既知の基準点
-- として使うために必要（式の途中でのユーザー関数呼び出し時のアライメント調整、genBody参照）
frameSize :: [Instr] -> Int
frameSize instrs = {-pTraceShow ("instrs:", instrs) $-} roundUp16 (maximum (0 : concatMap offsetOf instrs))
 where
  offsetOf (Load _ off) = [abs off]
  offsetOf (Store _ off) = [abs off]
  offsetOf (StoreArg _ _ off) = [abs off]
  offsetOf (LoadAddr off) = [abs off]
  offsetOf _ = []
  roundUp16 n = ((n + 15) `div` 16) * 16

prologueBody :: Int -> [String]
prologueBody size =
  [ "    pushq %rbp"
  , "    movq  %rsp, %rbp"
  ]
    ++ ["    subq  $" ++ show size ++ ", %rsp" | size > 0]

-- ユーザー定義関数の末尾: 操作スタックに残る唯一の戻り値を%raxへpopし、leave; retで戻る
-- （%raxはSysV AMD64の戻り値レジスタ。操作スタックは常に64bit正規化済みスロットのため、
-- 戻り値の論理型（i32/i64/bool）によらずpopq一本で正しい表現がそのまま取り出せる）
epilogueRet :: [String]
epilogueRet =
  [ "    popq  %rax"
  , "    leave"
  , "    ret"
  ]

-- mainの戻り値の型ごとに printf への引数の渡し方を切り替える（main専用）。
-- 整数型・ポインタ型はそのまま値を %rsi に渡すが、bool は 0/1 を "true"/"false" 文字列へ分岐させる。
epilogue :: Type -> [String]
epilogue (TyInt sg w) = printfEpilogue (fmtLabel sg w)
epilogue TUsize = printfEpilogue "fmtU64"
epilogue (TPtr _) = printfEpilogue "fmtPtr"
-- () は何も出力しない（Rust の main と同様）
epilogue TUnit =
  [ "    xorl  %eax, %eax"
  , "    leave"
  , "    ret"
  ]
-- 配列は値として扱えないため、末尾式の型が配列になることはない（compile側でエラーになる）
epilogue (TArray _ _) = error "epilogue: array type cannot be the type of the final expression"
epilogue TBool =
  [ "    popq  %rax"
  , "    andq  $-16, %rsp"
  , "    testq %rax, %rax"
  , "    jne   .Lbool_true"
  , "    leaq  strFalse(%rip), %rdi"
  , "    jmp   .Lbool_print"
  , ".Lbool_true:"
  , "    leaq  strTrue(%rip), %rdi"
  , ".Lbool_print:"
  , "    xorl  %eax, %eax"
  , "    call  printf"
  , "    xorl  %eax, %eax"
  , "    leave"
  , "    ret"
  ]

-- printf(fmtLabel, 値) の形で末尾式を出力する共通処理（整数型/ポインタで共有、フォーマットラベルのみ差し替える）
printfEpilogue :: String -> [String]
printfEpilogue label =
  [ "    popq  %rsi"
  , "    andq  $-16, %rsp"
  , "    leaq  " ++ label ++ "(%rip), %rdi"
  , "    xorl  %eax, %eax"
  , "    call  printf"
  , "    xorl  %eax, %eax"
  , "    leave"
  , "    ret"
  ]

-- ゼロ除算・配列の範囲外アクセス時のエラー処理とrodataセクションは全関数で共通、ファイル全体で一度だけ出力する
commonTail :: [String]
commonTail =
  [ ".Ldiv_zero_error:"
  , "    andq  $-16, %rsp"
  , "    leaq  errmsg(%rip), %rdi"
  , "    call  puts"
  , "    movl  $1, %edi"
  , "    call  exit"
  , ".Lindex_error:"
  , "    andq  $-16, %rsp"
  , "    leaq  idxerrmsg(%rip), %rdi"
  , "    call  puts"
  , "    movl  $1, %edi"
  , "    call  exit"
  , "    .section .rodata"
  , "fmt32:"
  , "    .string \"%d\\n\""
  , "fmt64:"
  , "    .string \"%ld\\n\""
  , "fmtU32:"
  , "    .string \"%u\\n\""
  , "fmtU64:"
  , "    .string \"%lu\\n\""
  , "fmtPtr:"
  , "    .string \"%p\\n\""
  , "strTrue:"
  , "    .string \"true\\n\""
  , "strFalse:"
  , "    .string \"false\\n\""
  , "errmsg:"
  , "    .string \"division by zero\""
  , "idxerrmsg:"
  , "    .string \"index out of bounds\""
  , "    .section .note.GNU-stack,\"\",@progbits"
  ]

fmtLabel :: Signedness -> Width -> String
fmtLabel Signed W32 = "fmt32"
fmtLabel Signed W64 = "fmt64"
fmtLabel Unsigned W32 = "fmtU32"
fmtLabel Unsigned W64 = "fmtU64"

-- 関数本体の命令列をアセンブリへ変換する。ICall（ユーザー関数呼び出し）だけは、
-- 呼び出し直前のスタックアライメントを静的に判定するために操作スタックの深さ
-- （8バイトスロット単位、プロローグ直後＝16バイト境界を0とする）を左から右へ辿りながら生成する。
-- 本プロジェクトのif/whileは常に文の境界（操作スタックが空）にしか出現しない
-- （if/while自体は値を返さない文であり、式の中に現れない）ため、分岐の合流点で
-- 深さが食い違うことはなく、単純な線形走査で静的に決定できる
genBody :: [Instr] -> [String]
genBody instrs = concat (snd (mapAccumL genAt 0 instrs))
 where
  genAt depth (ICall name n) = (depth + 1 - n, genCall depth n name)
  genAt depth instr = (depth + stackDelta instr, genInstr instr)

-- 各命令の操作スタックへの正味の増減（8バイトスロット単位）
stackDelta :: Instr -> Int
stackDelta (Push _) = 1
stackDelta (Load _ _) = 1
stackDelta (IAdd _) = -1
stackDelta (ISub _) = -1
stackDelta (IMul _) = -1
stackDelta (IDiv _) = -1
stackDelta (IDivU _) = -1
stackDelta (INeg _) = 0
stackDelta (Store _ _) = -1
stackDelta ICmpEq = -1
stackDelta ICmpNe = -1
stackDelta ICmpLt = -1
stackDelta ICmpLe = -1
stackDelta ICmpGt = -1
stackDelta ICmpGe = -1
stackDelta ICmpLtU = -1
stackDelta ICmpLeU = -1
stackDelta ICmpGtU = -1
stackDelta ICmpGeU = -1
stackDelta INot = 0
stackDelta (JmpIfZero _) = -1
stackDelta (Jmp _) = 0
stackDelta (Label _) = 0
stackDelta ISext32 = 0
stackDelta IZext32 = 0
stackDelta (LoadAddr _) = 1
stackDelta (LoadInd _) = 0
stackDelta (StoreArg _ _ _) = 0
stackDelta (IBoundsCheck _) = 0
stackDelta (StoreInd _) = -2
stackDelta (ICall _ n) = 1 - n
stackDelta IPop = -1

-- 呼び出し直前（＝n個の引数をレジスタへpopし終えた時点）の深さ d = depthAtCall - n が奇数なら、
-- 16バイト境界からrspが8バイトずれるため、call前後に8バイトのパディングを挟んで補正する
-- （frameSizeを16の倍数に切り上げてあるため、プロローグ直後は常に16バイト境界にあることが前提）
genCall :: Int -> Int -> String -> [String]
genCall depthAtCall n name =
  [ "    popq  " ++ argReg64 i | i <- reverse [0 .. n - 1]
  ]
    ++ ["    subq  $8, %rsp" | needsPad]
    ++ ["    call  " ++ name]
    ++ ["    addq  $8, %rsp" | needsPad]
    ++ ["    pushq %rax"]
 where
  needsPad = odd (depthAtCall - n)

-- SysV AMD64の整数引数レジスタ（0始まり、最大6個）
argReg64 :: Int -> String
argReg64 0 = "%rdi"
argReg64 1 = "%rsi"
argReg64 2 = "%rdx"
argReg64 3 = "%rcx"
argReg64 4 = "%r8"
argReg64 5 = "%r9"
argReg64 i = error ("argReg64: parameter index out of range: " ++ show i)

argReg32 :: Int -> String
argReg32 0 = "%edi"
argReg32 1 = "%esi"
argReg32 2 = "%edx"
argReg32 3 = "%ecx"
argReg32 4 = "%r8d"
argReg32 5 = "%r9d"
argReg32 i = error ("argReg32: parameter index out of range: " ++ show i)

genInstr :: Instr -> [String]
genInstr (Push n) =
  [ "    movabsq $" ++ show n ++ ", %rax"
  , "    pushq %rax"
  ]
genInstr (IAdd W64) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    addq  %rbx, %rax"
  , "    pushq %rax"
  ]
genInstr (IAdd W32) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    addl  %ebx, %eax"
  , "    cltq"
  , "    pushq %rax"
  ]
genInstr (ISub W64) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    subq  %rax, %rbx"
  , "    pushq %rbx"
  ]
genInstr (ISub W32) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    subl  %eax, %ebx"
  , "    movslq %ebx, %rbx"
  , "    pushq %rbx"
  ]
genInstr (IMul W64) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    imulq %rbx, %rax"
  , "    pushq %rax"
  ]
genInstr (IMul W32) =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    imull %ebx, %eax"
  , "    cltq"
  , "    pushq %rax"
  ]
-- 除数が-1のときは被除数の符号反転・除数1に置き換える。最小値/-1 で idiv が例外（#DE）を起こすのを避け、
-- 他の演算と同じく最小値へラップアラウンドさせる（negl/negq は最小値を自分自身へラップする）
genInstr (IDiv W64) =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    movq  %rax, %rdx"
  , "    negq  %rdx"
  , "    cmpq  $-1, %rcx"
  , "    cmoveq %rdx, %rax"
  , "    movq  $1, %rdx"
  , "    cmoveq %rdx, %rcx"
  , "    cqto"
  , "    idivq %rcx"
  , "    pushq %rax"
  ]
genInstr (IDiv W32) =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    movl  %eax, %edx"
  , "    negl  %edx"
  , "    cmpl  $-1, %ecx"
  , "    cmovel %edx, %eax"
  , "    movl  $1, %edx"
  , "    cmovel %edx, %ecx"
  , "    cltd"
  , "    idivl %ecx"
  , "    cltq"
  , "    pushq %rax"
  ]
-- 符号なし除算。被除数の上位（%rdx/%edx）をゼロにして divq/divl する（符号なしでは最小値/-1 の問題は起きない）。
-- divl の結果を %eax へ書くと上位32bitは自動的にゼロになるため、u32 のゼロ拡張済みの値がそのまま得られる
genInstr (IDivU W64) =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    xorl  %edx, %edx"
  , "    divq  %rcx"
  , "    pushq %rax"
  ]
genInstr (IDivU W32) =
  [ "    popq  %rcx"
  , "    cmpl  $0, %ecx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    xorl  %edx, %edx"
  , "    divl  %ecx"
  , "    pushq %rax"
  ]
genInstr (INeg W64) =
  [ "    popq  %rax"
  , "    negq  %rax"
  , "    pushq %rax"
  ]
genInstr (INeg W32) =
  [ "    popq  %rax"
  , "    negl  %eax"
  , "    cltq"
  , "    pushq %rax"
  ]
genInstr (Load W64 off) =
  [ "    movq  " ++ show off ++ "(%rbp), %rax"
  , "    pushq %rax"
  ]
genInstr (Load W32 off) =
  [ "    movslq " ++ show off ++ "(%rbp), %rax"
  , "    pushq %rax"
  ]
genInstr (Store W64 off) =
  [ "    popq  %rax"
  , "    movq  %rax, " ++ show off ++ "(%rbp)"
  ]
genInstr (Store W32 off) =
  [ "    popq  %rax"
  , "    movl  %eax, " ++ show off ++ "(%rbp)"
  ]
genInstr ICmpEq =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    sete  %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpNe =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    setne %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpLt =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    setl  %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpLe =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    setle %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpGt =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    setg  %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpGe =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    setge %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
genInstr ICmpLtU = cmpU "setb "
genInstr ICmpLeU = cmpU "setbe"
genInstr ICmpGtU = cmpU "seta "
genInstr ICmpGeU = cmpU "setae"
genInstr INot =
  [ "    popq  %rax"
  , "    xorq  $1, %rax"
  , "    pushq %rax"
  ]
genInstr (JmpIfZero lbl) =
  [ "    popq  %rax"
  , "    testq %rax, %rax"
  , "    je    " ++ lbl
  ]
genInstr (Jmp lbl) =
  [ "    jmp   " ++ lbl
  ]
genInstr (Label lbl) =
  [ lbl ++ ":"
  ]
genInstr ISext32 =
  [ "    popq  %rax"
  , "    cltq"
  , "    pushq %rax"
  ]
genInstr IZext32 =
  [ "    popq  %rax"
  , "    movl  %eax, %eax"
  , "    pushq %rax"
  ]
genInstr (LoadAddr off) =
  [ "    leaq  " ++ show off ++ "(%rbp), %rax"
  , "    pushq %rax"
  ]
genInstr (LoadInd W64) =
  [ "    popq  %rax"
  , "    movq  (%rax), %rax"
  , "    pushq %rax"
  ]
genInstr (LoadInd W32) =
  [ "    popq  %rax"
  , "    movslq (%rax), %rax"
  , "    pushq %rax"
  ]
-- 添字（スタック先頭）はpopせずに覗き見る。符号なしで比較するため、負の値を
-- 符号なしへ再解釈した巨大な添字も範囲外として検出される
genInstr (IBoundsCheck n) =
  [ "    movq  (%rsp), %rax"
  , "    movabsq $" ++ show n ++ ", %rcx"
  , "    cmpq  %rcx, %rax"
  , "    jae   .Lindex_error"
  ]
genInstr (StoreInd W64) =
  [ "    popq  %rax"
  , "    popq  %rcx"
  , "    movq  %rax, (%rcx)"
  ]
genInstr (StoreInd W32) =
  [ "    popq  %rax"
  , "    popq  %rcx"
  , "    movl  %eax, (%rcx)"
  ]
genInstr (StoreArg i W64 off) =
  [ "    movq  " ++ argReg64 i ++ ", " ++ show off ++ "(%rbp)"
  ]
genInstr (StoreArg i W32 off) =
  [ "    movl  " ++ argReg32 i ++ ", " ++ show off ++ "(%rbp)"
  ]
genInstr IPop =
  [ "    addq  $8, %rsp"
  ]
genInstr (ICall _ _) =
  error "ICall requires operand-stack-depth context; must be generated via genBody, not genInstr"

-- 符号なし大小比較（setb/setbe/seta/setae は CF/ZF を見る。u32 はゼロ拡張済みなので64bitで比較してよい）
cmpU :: String -> [String]
cmpU setcc =
  [ "    popq  %rbx"
  , "    popq  %rax"
  , "    cmpq  %rbx, %rax"
  , "    " ++ setcc ++ " %al"
  , "    movzbq %al, %rax"
  , "    pushq %rax"
  ]
