module XMonad.Prompt.Hints
  ( hintsPromptFocus
  , defaultHConfig
  ) where

import Control.Concurrent

import XMonad
import XMonad.Prompt
import XMonad.Util.Font
import XMonad.Util.Hints
import XMonad.Util.XUtils

data HintConfig = HintConfig
  { hintChar :: String
  , hintFont :: String
  , hintTitleLen :: Int
  }

defaultHConfig = HintConfig
  { hintChar = "jfnvuthgbymridkcoelspwaqz"
  , hintFont = "fixed"
  , hintTitleLen = -1
  }

data HintsPrompt = Focus
instance XPrompt HintsPrompt where
  showXPrompt Focus = "Focus window: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

hintsPromptFocus :: HintConfig -> XPConfig -> X ()
hintsPromptFocus = doPrompt Focus

doPrompt :: HintsPrompt -> HintConfig -> XPConfig -> X ()
doPrompt t h c = do
  dpy <- asks display
  xmf <- initXMF $ hintFont h
  wm <- windowMap $ hintChar h
  hs <- createHints xmf wm
  let ws = map (\(x, y) -> y) hs
  showWindows ws
  drawHints dpy xmf hs
  a <- case t of
    Focus -> return $ focusAction wm
  mkXPrompt t c (\_ -> return []) a
  deleteWindows ws
  releaseXMF xmf

focusAction :: [(String, Window)] -> String -> X ()
focusAction wm s = do
  let mw = lookup s wm
  case mw of
    Nothing -> return ()
    Just w -> focus w

createHints :: XMonadFont -> [(String, Window)] -> X ([(String, Window)])
createHints xmf [] = return []
createHints xmf (w:ws) = do
  p <- createHint xmf w
  ps <- createHints xmf ws
  return (p:ps)

createHint :: XMonadFont -> (String, Window) -> X (String, Window)
createHint xmf (str, win) = do
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

