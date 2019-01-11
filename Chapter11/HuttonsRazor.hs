data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Add e1 e2) = eval e1 + eval e2
eval (Lit n) = n

printExpr :: Expr -> String
printExpr (Add e1 e2) = concat [printExpr e1, " + ", printExpr e2]
printExpr (Lit n) = show n


main = do
  print $ eval (Add (Lit 1) (Lit 9001))
  print $ printExpr (Add (Lit 1) (Lit 9001))
