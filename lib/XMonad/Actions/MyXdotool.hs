module XMonad.Actions.MyXdotool
  ( Xdotool (..)
  , runXdotool
  ) where

import XMonad

data Xdotool = MousemoveRelative Int Int
             | Click Int
             | Mousedown Int
             | Mouseup Int

runXdotool :: Xdotool -> X ()
runXdotool t = spawn $ "xdotool " ++ cmd t
  where cmd (MousemoveRelative x y) = "mousemove_relative -- " ++ show x ++ " " ++ show y
        cmd (Click n) = "click " ++ show n
        cmd (Mousedown n) = "mousedown " ++ show n
        cmd (Mouseup n) = "mouseup " ++ show n
