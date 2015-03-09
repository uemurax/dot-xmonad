module XMonad.Hints
( hints
) where

import XMonad hiding (initColor)
import qualified XMonad.StackSet as W
import Control.Concurrent (threadDelay)

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

printString :: Display -> Drawable -> GC -> FontStruct -> String -> IO ()
printString dpy d gc fontst str = do
  let strLen = textWidth fontst str
      valign = 10
      remWidth = 20 - strLen
      offset = remWidth `div` 2
  fgcolor <- initColor dpy "black"
  bgcolor <- initColor dpy "white"
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  drawImageString dpy d gc offset valign str

currentWorkspace :: W.StackSet i l a s sd -> W.Workspace i l a
currentWorkspace = W.workspace . W.current

currentWindows :: W.StackSet i l a s sd -> [a]
currentWindows = W.integrate' . W.stack . currentWorkspace

createPanel :: Display -> Window -> IO Window
createPanel dpy win = do
  let dflt = defaultScreen dpy
      border = blackPixel dpy dflt
      background = whitePixel dpy dflt
  pnl <- createSimpleWindow dpy win 0 0 10 12 1 border background
  return pnl

drawPanel :: Display -> Window -> String -> IO ()
drawPanel dpy pnl str = do
  gc <- createGC dpy pnl
  fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-24-*-*-*-*-*-*-*"
  printString dpy pnl gc fontStruc str
  freeGC dpy gc
  freeFont dpy fontStruc

drawPanels :: Display -> [(String, Window)] -> IO ()
drawPanels dpy [] = return ()
drawPanels dpy ((s, p):ps) = do
  drawPanel dpy p s
  drawPanels dpy ps

createPanels :: Display -> [Window] -> IO [Window]
createPanels dpy [] = return []
createPanels dpy (w:ws) = do
  p <- createPanel dpy w
  ps <- createPanels dpy ws
  return (p:ps)

destroyPanels :: Display -> [Window] -> IO ()
destroyPanels dpy [] = return ()
destroyPanels dpy (p:ps) = do
  destroyWindow dpy p
  destroyPanels dpy ps

mapPanels :: Display -> [Window] -> IO ()
mapPanels dpy [] = return ()
mapPanels dpy (p:ps) = do
  mapWindow dpy p
  mapPanels dpy ps

showPanels :: Display -> [(String, Window)] -> IO ()
showPanels dpy ws = do
  let (strs, wins) = (map fst ws, map snd ws)
  ps <- createPanels dpy wins
  mapPanels dpy ps
  drawPanels dpy $ zip strs ps
  sync dpy False
  threadDelay (5 * 1000000)
  destroyPanels dpy ps

hints :: [String] -> X ()
hints strs = withDisplay (\dpy ->
  withWindowSet (\stk ->
    io . showPanels dpy . zip strs . currentWindows $ stk))

