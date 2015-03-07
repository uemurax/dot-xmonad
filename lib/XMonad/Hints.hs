module XMonad.Hints
( drawInFocusedWindowX
) where

import XMonad hiding (initColor)
import qualified XMonad.StackSet as W

drawInFocusedWindowX :: String -> X ()
drawInFocusedWindowX str = withDisplay (\dpy ->
  withFocused (\win ->
    io $ drawInWin dpy win str))

drawInWin :: Display -> Window -> String -> IO ()
drawInWin dpy win str = do
  bgcolor <- initColor dpy "green"
  fgcolor <- initColor dpy "blue"
  gc <- createGC dpy win
  fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-32-*-*-*-*-*-*-*"
  --setForeground dpy gc bgcolor
  --fillRectangle dpy win gc 0 0 200 100
  --setForeground dpy gc fgcolor
  --fillRectangle dpy win gc 2 2 196 196
  printString dpy win gc fontStruc str
  freeGC dpy gc
  freeFont dpy fontStruc

printString :: Display
  -> Drawable
  -> GC
  -> FontStruct
  -> String
  -> IO ()
printString dpy d gc fontst str = do
  let strLen = textWidth fontst str
      (_, asc, _, _) = textExtents fontst str
      valign = (5 + fromIntegral asc) `div` 2
      remWidth = 10
      offset = remWidth `div` 2
  fgcolor <- initColor dpy "black"
  bgcolor <- initColor dpy "yellow"
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  drawImageString dpy d gc offset valign str

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

