module CodeGen (codegen) where

import Compiler (Instr (..), Type (..), Width (..))

codegen :: Type -> [Instr] -> String
codegen finalType instrs =
  unlines $
    prologue (frameSize instrs)
      ++ concatMap genInstr instrs
      ++ epilogue finalType

-- ローカル変数用に確保するスタックフレームのサイズ（バイト）。
-- Load/Store が保持する %rbp 相対オフセットの絶対値の最大が必要な確保量になる。
frameSize :: [Instr] -> Int
frameSize instrs = maximum (0 : concatMap offsetOf instrs)
  where
    offsetOf (Load _ off) = [abs off]
    offsetOf (Store _ off) = [abs off]
    offsetOf _ = []

prologue :: Int -> [String]
prologue size =
  [ "    .section .text"
  , "    .globl main"
  , "main:"
  , "    pushq %rbp"
  , "    movq  %rsp, %rbp"
  ]
    ++ ["    subq  $" ++ show size ++ ", %rsp" | size > 0]

-- 末尾式の型ごとに printf への引数の渡し方を切り替える。
-- 整数型はそのまま値を %rsi に渡すが、bool は 0/1 を "true"/"false" 文字列へ分岐させる。
epilogue :: Type -> [String]
epilogue (TyInt w) =
  [ "    popq  %rsi"
  , "    andq  $-16, %rsp"
  , "    leaq  " ++ fmtLabel w ++ "(%rip), %rdi"
  , "    xorl  %eax, %eax"
  , "    call  printf"
  , "    xorl  %eax, %eax"
  , "    leave"
  , "    ret"
  ]
    ++ commonTail
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
    ++ commonTail

-- ゼロ除算時のエラー処理とrodataセクションは末尾式の型によらず共通
commonTail :: [String]
commonTail =
  [ ".Ldiv_zero_error:"
  , "    andq  $-16, %rsp"
  , "    leaq  errmsg(%rip), %rdi"
  , "    call  puts"
  , "    movl  $1, %edi"
  , "    call  exit"
  , "    .section .rodata"
  , "fmt32:"
  , "    .string \"%d\\n\""
  , "fmt64:"
  , "    .string \"%ld\\n\""
  , "strTrue:"
  , "    .string \"true\\n\""
  , "strFalse:"
  , "    .string \"false\\n\""
  , "errmsg:"
  , "    .string \"division by zero\""
  , "    .section .note.GNU-stack,\"\",@progbits"
  ]

fmtLabel :: Width -> String
fmtLabel W32 = "fmt32"
fmtLabel W64 = "fmt64"

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
genInstr (IDiv W64) =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    cqto"
  , "    idivq %rcx"
  , "    pushq %rax"
  ]
genInstr (IDiv W32) =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    cltd"
  , "    idivl %ecx"
  , "    cltq"
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
genInstr INot =
  [ "    popq  %rax"
  , "    xorq  $1, %rax"
  , "    pushq %rax"
  ]
genInstr ISext32 =
  [ "    popq  %rax"
  , "    cltq"
  , "    pushq %rax"
  ]
