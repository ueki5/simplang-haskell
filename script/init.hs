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
-- source = "let a = 5;a" -- 何もない
-- source = "fn add(a:i64, b){a + b} add(2, 4)" -- 仮引数のみ
-- source = "fn add(a, b){a + b + 8i64} add(2, 4)" -- 内部ロジックからの推論はしない
-- source = "fn add(a, b){a + b} add(2i64, 4i64)" -- 末尾式から
-- source = "fn add(a, b){a + b} let c = add(2i64, 4i64);c" -- 暗黙mainのCall文から
-- source = "fn add(a, b){a + b} let c = add(2i64, 4);c" -- 暗黙mainのCall文から（片側未定）
source = "fn add(a, b){a + b} let c:i64 = add(2, 4);c" -- 暗黙mainのCall文から（片側未定）
-- source = "fn add(a, b:i64){ let c = a + b + 0i64; c } let d = 5i64; let e = d * 2; add(d, e)"
("source", source)
tokenize source >>= parse >>= compile
