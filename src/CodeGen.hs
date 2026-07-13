module CodeGen (codegen) where

import Compiler (Instr (..), Width (..))

codegen :: Width -> [Instr] -> String
codegen finalWidth instrs =
  unlines $
    prologue (frameSize instrs)
      ++ concatMap genInstr instrs
      ++ epilogue finalWidth

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

epilogue :: Width -> [String]
epilogue finalWidth =
  [ "    popq  %rsi"
  , "    andq  $-16, %rsp"
  , "    leaq  " ++ fmtLabel finalWidth ++ "(%rip), %rdi"
  , "    xorl  %eax, %eax"
  , "    call  printf"
  , "    xorl  %eax, %eax"
  , "    leave"
  , "    ret"
  , ".Ldiv_zero_error:"
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
