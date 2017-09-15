module XMonad.Util.MyList
  ( enumWords
  ) where

diag :: [a] -> [[a]]
diag l = map (\x -> [x]) l ++ [x:xs | xs <- diag l, x <- l]

enumWords :: [a] -> [[a]]
enumWords l = map reverse $ diag l

