{-# LANGUAGE FlexibleContexts #-}

module XMonad.Config.MyDocks
  ( myDocks
  ) where

import XMonad
import XMonad.Hooks.ManageDocks

myDocks cfg = docks cfg { layoutHook = avoidStruts $ layoutHook cfg }
