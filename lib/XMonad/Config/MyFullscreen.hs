{-# LANGUAGE FlexibleContexts #-}

module XMonad.Config.MyFullscreen
  ( myFullscreen
  ) where

import XMonad
import XMonad.Layout.Fullscreen

myFullscreen cfg =
  cfg { handleEventHook = handleEventHook cfg <+> fullscreenEventHook
      , manageHook =  manageHook cfg <+> fullscreenManageHook
      , layoutHook = fullscreenFocus  $ layoutHook cfg
      }
