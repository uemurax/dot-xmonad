module XMonad.Config.MyMate (
  myMate
  ) where

import XMonad
import XMonad.Config.Mate

myMate cfg =
  cfg { terminal = "mate-terminal"
      , startupHook = mateRegister >> startupHook cfg
      }
