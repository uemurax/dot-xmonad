module XMonad.Util.Misc
  ( swapWith
  , maximizeWindow
  ) where

import XMonad
import qualified XMonad.StackSet as W

-- | swap the current focused window and the specified window and focus the specified window
swapWith :: (Eq a) =>
  a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swapWith = W.modify' . swapWith'

swapWith' :: (Eq a) =>
  a -> W.Stack a -> W.Stack a
swapWith' a stk@(W.Stack t ls rs) =
  if a == t then stk
  else case findWindow a ls rs of
    Nothing -> stk
    Just (Left (ll, lr)) -> W.Stack t ll (lr ++ [a] ++ rs)
    Just (Right (rl, rr)) -> W.Stack t (rr ++ [a] ++ ls) rl

findWindow :: (Eq a) => a -> [a] -> [a] -> Maybe (Either ([a], [a]) ([a], [a]))
findWindow a ls rs = case findZ a (ls, []) of
  Nothing -> case findZ a (rs, []) of
    Nothing -> Nothing
    Just r -> Just (Right r)
  Just l -> Just (Left l)

findZ :: (Eq a) => a -> ([a], [a]) -> Maybe ([a], [a])
findZ a ([], _) = Nothing
findZ a (x:xs, ys) =
  if a == x then Just (xs, ys)
  else findZ a (xs, x:ys)

maximizeWindow :: Window -> X ()
maximizeWindow win = withDisplay $ \d -> do
  let s = defaultScreen d
      w = fromIntegral $ displayWidth d s
      h = fromIntegral $ displayHeight d s
  io $ moveResizeWindow d win 0 0 w h
  float win

