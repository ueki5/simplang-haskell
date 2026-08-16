# 関数名見直し

- parseExprをparseAdditiveへ変更（関数名を本来の意味に合わせる）
- parseExprRestをparseAdditiveRestへ変更（関数名を本来の意味に合わせる）
- parseEqualityは、式をパースする際の入口だが、直接呼び出されているため、parseExprを新設し、新たな入口とする
- parseTermをparseMultiplicativeへ変更（関数名を本来の意味に合わせる）
- parseTermRestをparseMultiplicativeRestへ変更（関数名を本来の意味に合わせる）
- **文法**:
  ```
  expr           ::= equality
  equality       ::= comparison (('==' | '!=') comparison)*
  comparison     ::= additive (('<' | '<=' | '>' | '>=') additive)*
  additive       ::= multiplicative (('+' | '-') multiplicative)*
  multiplicative ::= factor (('*' | '/') factor)*
