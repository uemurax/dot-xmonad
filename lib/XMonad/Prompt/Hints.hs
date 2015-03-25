module XMonad.Prompt.Hints
  ( HintAction (..)
  , HintPrompt (..)
  , HintConfig (..)
  , hintPrompt
  , defaultHConfig
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Util.Font
import XMonad.Util.NamedWindows
import XMonad.Util.Hints
import XMonad.Util.XUtils
import XMonad.Util.Misc

class (XPrompt a) => HintAction a where
  hintAction :: a -> Window -> X ()

data HintConfig = HintConfig
  { hintChar :: String
  , hintFont :: String
  , hintFgColor :: String
  , hintBgColor :: String
  , hintTitleLen :: Int
  }

defaultHConfig = HintConfig
  { hintChar = "jfnvuthgbymridkcoelspwaqz"
  , hintFont = "-misc-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
  , hintFgColor = "#000000"
  , hintBgColor = "#f0f040"
  , hintTitleLen = 20
  }

data HintPrompt = Focus | BringToMaster | Close | BringToWS String
                | Swap | Float | Sink

instance XPrompt HintPrompt where
  showXPrompt Focus = "Focus window: "
  showXPrompt BringToMaster = "Bring to master: "
  showXPrompt Close = "Close: "
  showXPrompt (BringToWS i) = "Bring to workspace " ++ i ++ ": "
  showXPrompt Swap = "Swap: "
  showXPrompt Float = "Float: "
  showXPrompt Sink = "Sink: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

instance HintAction HintPrompt where
  hintAction Focus w = focus w
  hintAction BringToMaster w = do
    focus w
    windows W.swapMaster
  hintAction Close w = killWindow w
  hintAction (BringToWS i) w = do
    windows $ W.shiftWin i w
  hintAction Swap w = windows $ swapWith w
  hintAction Float w = float w
  hintAction Sink w = windows $ W.sink w

hintPrompt :: (HintAction a) => a -> HintConfig -> XPConfig -> X ()
hintPrompt t h c = do
  let titleLen = hintTitleLen h
      fg = hintFgColor h
      bg = hintBgColor h
  dpy <- asks display
  xmf <- initXMF $ hintFont h
  wm <- windowMap $ hintChar h
  hs <- createHints xmf titleLen wm
  let ws = map (\(x, y) -> y) hs
  showWindows ws
  drawHints dpy xmf fg bg hs
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
      str = " " ++ str' ++ ": " ++ str''' ++ " "
  dpy <- asks display
  width <- textWidthXMF dpy xmf str
  (ascent, descent) <- textExtentsXMF xmf str
  (rootw, x, y, w, h, b, d) <- io $ getGeometry dpy win
  let x' = x + fi ((fi w - width) `div` 2)
      height = ascent + descent
      y' = y + fi ((fi h - height) `div` 2)
  ret <- createNewWindow (Rectangle x' y' (fi width) (fi height)) Nothing "" False
  return (str, ret)

drawHint :: Display -> XMonadFont -> String -> String -> (String, Window) -> X ()
drawHint dpy xmf fg bg (str, win) = do
  gc <- io $ createGC dpy win
  (ascent, descent) <- textExtentsXMF xmf str 
  printStringXMF dpy win xmf gc fg bg 0 ascent str
  io $ freeGC dpy gc

drawHints :: Display -> XMonadFont -> String -> String -> [(String, Window)] -> X ()
drawHints dpy xmf fg bg [] = return ()
drawHints dpy xmf fg bg (w:ws) = do
  drawHint dpy xmf fg bg w
  drawHints dpy xmf fg bg ws

