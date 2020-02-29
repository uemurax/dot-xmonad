{-# LANGUAGE FlexibleContexts #-}

module XMonad.Config.MyMaximize
  ( myMaximize
  ) where

import XMonad
import XMonad.Layout.Maximize
import XMonad.Util.EZConfig

myMaximize cfg =
  cfg { layoutHook = maximize $ layoutHook cfg
      } `additionalKeysP`
  [ ("M-z", withFocused $ sendMessage . maximizeRestore)
  ]
