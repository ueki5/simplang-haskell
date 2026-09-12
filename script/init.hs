-- Parser, Compiler, CodeGen は常にソースからロードする（非公開資産を参照可能にするため）
-- 複数ターゲットを:lすると、GHCiはコンテキストを自動設定しない（単一ターゲットなら*Moduleが自動設定される）ため、
-- 非公開識別子（initialSlotsなど）を無修飾で使うには明示的に*付きでモジュールをコンテキストに追加する必要がある

:set -Wno-type-defaults -Wno-unused-imports -Wno-deprecations
:l src/Parser.hs src/CodeGen.hs src/Compiler.hs
:m *Parser *CodeGen *Compiler
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
-- :{
-- source = [r|
-- fn add(a, b:i64){
--   let c = a + b + 0i64;
--   c
-- }
-- let d = 5i64;
-- let e = d * 2;
-- add(d, e)
-- |]::String
-- :}
-- source = "let a = 5;a"
-- source = "fn add(a:i64, b){a + b} add(2, 4)"
-- source = "fn add(a, b){a + b + 8i64} add(2, 4)"
-- source = "fn add(a, b){a + b} add(2i64, 4i64)"
-- source = "fn add(a, b){a + b} let c = add(2i64, 4i64);c"
-- source = "fn add(a, b){a + b} let c = add(2i64, 4);c"
-- source = "fn add(a, b){a + b} let c:i65 = add(2, 4);c"
-- source = "fn add(a, b){a + b} fn sub(a, b){a - b}  let c = add(2, 4); let d = sub(c, 4);c"
-- source = "fn add(a, b){a + b} fn sub(a, b){a - b}  let c:i64 = add(2, 4); let d = sub(c, 4i64);c"
source = "fn add(a, b){a + b} fn sub(a, b){a - b}  let c:i32 = add(2, 4); let d:i32 = sub(c, 4);c"
-- source = "fn add(a, b:i64){ let c = a + b + 0i64; c } let d = 5i64; let e = d * 2; add(d, e)"
tokens = tokenize source
parsed = tokens >>= parse
compld = parsed >>= compile
codegen' = \(fns, finalType, instrs) -> Right (codegen fns finalType instrs)
asmblr = compld >>= codegen'
eithlen = (\input -> either (\_ -> 0) (length . id) input)::Either String String -> Int
("アセンブラ文字数", eithlen asmblr) 
("ソースコード内容", source)
"途中経過は ((\"tokens\", tokens), (\"parsed\", parsed), (\"compld\", compld), (\"asmblr\", asmblr))"
