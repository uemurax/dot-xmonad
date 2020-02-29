module XMonad.Config.MyMate (
  myMate
  ) where

import XMonad
import XMonad.Config.Mate
import XMonad.Util.EZConfig

myMate cfg =
  cfg { terminal = "mate-terminal"
      , startupHook = mateRegister >> startupHook cfg
      } `additionalKeysP`
  [ ("M-S-q", spawn "mate-session-save --logout-dialog")
  ]
