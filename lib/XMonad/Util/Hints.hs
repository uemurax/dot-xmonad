module XMonad.Util.Hints
  ( windowMap
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.List

windowMap' :: W.StackSet i l a s sd -> [String] -> [(String, a)]
windowMap' stk ss = zip ss $ W.index stk

windowMap'' :: [String] -> X ([(String, Window)])
windowMap'' ss = withWindowSet (\ws -> return (windowMap' ws ss))

windowMap :: String -> X ([(String, Window)])
windowMap s = windowMap'' $ enumWords s

