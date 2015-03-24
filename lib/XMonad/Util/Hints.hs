module XMonad.Util.Hints
  ( windowMap
  ) where

import XMonad
import qualified XMonad.StackSet as W

diag :: [a] -> [[a]]
diag l = map (\x -> [x]) l ++ [x:xs | xs <- diag l, x <- l]

mkHintStr :: [a] -> [[a]]
mkHintStr l = map reverse $ diag l

windowMap' :: W.StackSet i l a s sd -> [String] -> [(String, a)]
windowMap' stk ss = zip ss $ W.index stk

windowMap'' :: [String] -> X ([(String, Window)])
windowMap'' ss = withWindowSet (\ws -> return (windowMap' ws ss))

windowMap :: String -> X ([(String, Window)])
windowMap s = windowMap'' $ mkHintStr s

