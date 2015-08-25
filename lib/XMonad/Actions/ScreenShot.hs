module XMonad.Actions.ScreenShot
       ( takeScreenShot
       ) where

import XMonad

takeScreenShot :: (Window -> X String) -> Window -> X ()
takeScreenShot c w = do
  filename <- c w
  spawn $ "import -window " ++ show w ++ " " ++ filename
