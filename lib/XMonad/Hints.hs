module XMonad.Hints
( hints0
) where

import XMonad hiding (initColor)
import qualified XMonad.StackSet as W
import Control.Concurrent (threadDelay)

currentWorkspace :: W.StackSet i l a s sd -> W.Workspace i l a
currentWorkspace = W.workspace . W.current

currentWindows :: W.StackSet i l a s sd -> [a]
currentWindows = W.integrate' . W.stack . currentWorkspace

createPanel :: Display -> Window -> IO Window
createPanel dpy win = do
  let dflt = defaultScreen dpy
      border = blackPixel dpy dflt
      background = whitePixel dpy dflt
  createSimpleWindow dpy win 0 0 30 30 1 border background

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

showPanels :: Display -> [Window] -> IO ()
showPanels dpy ws = do
  ps <- createPanels dpy ws
  mapPanels dpy ps
  sync dpy False
  threadDelay (5 * 1000000)
  destroyPanels dpy ps

hints0 :: X ()
hints0 = withDisplay (\dpy ->
  withWindowSet (\stk ->
    io $ showPanels dpy (currentWindows stk)))

