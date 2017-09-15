module XMonad.Util.MyMisc
  ( maximizeWindow
  ) where

import XMonad

maximizeWindow :: Window -> X ()
maximizeWindow win = withDisplay $ \d -> do
  let s = defaultScreen d
      w = fromIntegral $ displayWidth d s
      h = fromIntegral $ displayHeight d s
  io $ moveResizeWindow d win 0 0 w h
  float win
