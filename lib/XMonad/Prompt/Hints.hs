module XMonad.Prompt.Hints
  ( hintsPromptFocus
  , hintsPromptBringToMaster
  , defaultHConfig
  ) where

import Control.Concurrent

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Util.Font
import XMonad.Util.NamedWindows
import XMonad.Util.Hints
import XMonad.Util.XUtils

data HintConfig = HintConfig
  { hintChar :: String
  , hintFont :: String
  , hintTitleLen :: Int
  }

defaultHConfig = HintConfig
  { hintChar = "jfnvuthgbymridkcoelspwaqz"
  , hintFont = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
  , hintTitleLen = -1
  }

data HintsPrompt = Focus | BringToMaster
instance XPrompt HintsPrompt where
  showXPrompt Focus = "Focus window: "
  showXPrompt BringToMaster = "Bring to master: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

hintAction :: HintsPrompt -> Window -> X ()
hintAction Focus w = focus w
hintAction BringToMaster w = do
  focus w
  windows W.swapMaster

hintsPromptFocus = doPrompt Focus
hintsPromptBringToMaster = doPrompt BringToMaster

doPrompt :: HintsPrompt -> HintConfig -> XPConfig -> X ()
doPrompt t h c = do
  let titleLen = hintTitleLen h
  dpy <- asks display
  xmf <- initXMF $ hintFont h
  wm <- windowMap $ hintChar h
  hs <- createHints xmf titleLen wm
  let ws = map (\(x, y) -> y) hs
  showWindows ws
  drawHints dpy xmf hs
  mkXPrompt t c (\_ -> return []) $ \s -> do
    let mw = lookup s wm
    case mw of
      Nothing -> return ()
      Just w -> hintAction t w
  deleteWindows ws
  releaseXMF xmf

createHints :: XMonadFont -> Int -> [(String, Window)] -> X ([(String, Window)])
createHints xmf n [] = return []
createHints xmf n (w:ws) = do
  p <- createHint xmf n w
  ps <- createHints xmf n ws
  return (p:ps)

createHint :: XMonadFont -> Int -> (String, Window) -> X (String, Window)
createHint xmf n (str', win) = do
  nw <- getName win
  let str'' = show nw
      str''' = if n < 0 then str''
            else take n str''
      str = str' ++ ": " ++ str'''
  dpy <- asks display
  width <- textWidthXMF dpy xmf str
  (ascent, descent) <- textExtentsXMF xmf str
  (rootw, x, y, w, h, b, d) <- io $ getGeometry dpy win
  let x' = x + fi ((fi w - width) `div` 2)
      height = ascent + descent
      y' = y + fi ((fi h - height) `div` 2)
  ret <- createNewWindow (Rectangle x' y' (fi width) (fi height)) Nothing "" False
  return (str, ret)

drawHint :: Display -> XMonadFont -> (String, Window) -> X ()
drawHint dpy xmf (str, win) = do
  gc <- io $ createGC dpy win
  (ascent, descent) <- textExtentsXMF xmf str 
  printStringXMF dpy win xmf gc "black" "yellow" 0 ascent str
  io $ freeGC dpy gc

drawHints :: Display -> XMonadFont -> [(String, Window)] -> X ()
drawHints dpy xmf [] = return ()
drawHints dpy xmf (w:ws) = do
  drawHint dpy xmf w
  drawHints dpy xmf ws

