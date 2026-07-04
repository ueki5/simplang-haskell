module CodeGen (codegen) where

import Compiler (Instr (..))

codegen :: [Instr] -> String
codegen instrs =
  unlines $
    prologue (frameSize instrs)
      ++ concatMap genInstr instrs
      ++ epilogue

-- ローカル変数用に確保するスタックフレームのサイズ（バイト）。
-- Load/Store が保持する %rbp 相対オフセットの絶対値の最大が必要な確保量になる。
frameSize :: [Instr] -> Int
frameSize instrs = maximum (0 : concatMap offsetOf instrs)
  where
    offsetOf (Load off) = [abs off]
    offsetOf (Store off) = [abs off]
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

epilogue :: [String]
epilogue =
  [ "    popq  %rsi"
  , "    andq  $-16, %rsp"
  , "    leaq  fmt(%rip), %rdi"
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
  , "fmt:"
  , "    .string \"%d\\n\""
  , "errmsg:"
  , "    .string \"division by zero\""
  , "    .section .note.GNU-stack,\"\",@progbits"
  ]

genInstr :: Instr -> [String]
genInstr (Push n) =
  [ "    pushq $" ++ show n
  ]
genInstr IAdd =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    addq  %rbx, %rax"
  , "    pushq %rax"
  ]
genInstr ISub =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    subq  %rax, %rbx"
  , "    pushq %rbx"
  ]
genInstr IMul =
  [ "    popq  %rax"
  , "    popq  %rbx"
  , "    imulq %rbx, %rax"
  , "    pushq %rax"
  ]
genInstr IDiv =
  [ "    popq  %rcx"
  , "    cmpq  $0, %rcx"
  , "    je    .Ldiv_zero_error"
  , "    popq  %rax"
  , "    cqto"
  , "    idivq %rcx"
  , "    pushq %rax"
  ]
genInstr INeg =
  [ "    popq  %rax"
  , "    negq  %rax"
  , "    pushq %rax"
  ]
genInstr (Load off) =
  [ "    movq  " ++ show off ++ "(%rbp), %rax"
  , "    pushq %rax"
  ]
genInstr (Store off) =
  [ "    popq  %rax"
  , "    movq  %rax, " ++ show off ++ "(%rbp)"
  ]
