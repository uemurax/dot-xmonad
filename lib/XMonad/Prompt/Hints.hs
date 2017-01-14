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

import Data.Default
import Data.List
import Data.Char (toUpper)

class (XPrompt a) => HintAction a where
  hintAction :: a -> Window -> X ()

data HintConfig = HintConfig
  { hintChar :: String
  , hintFont :: String
  , hintFgColor :: String
  , hintBgColor :: String
  , hintTitleLen :: Int
  , hintLayout :: X (Maybe (Layout Window))
  }

defaultHConfig = HintConfig
  { hintChar = "jfkdlsahg"
  , hintFont = "-misc-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
  , hintFgColor = "#000000"
  , hintBgColor = "#f0f040"
  , hintTitleLen = 20
  , hintLayout = return Nothing
  }

instance Default HintConfig where
  def = defaultHConfig

data HintPrompt = Focus

instance XPrompt HintPrompt where
  showXPrompt Focus = "Focus window: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

instance HintAction HintPrompt where
  hintAction Focus w = focus w

hintPrompt :: (HintAction a) => a -> HintConfig -> XPConfig -> X ()
hintPrompt t h c = do
  let titleLen = hintTitleLen h
      fg = hintFgColor h
      bg = hintBgColor h
  l <- hintLayout h
  currentLayout <- dumpLayout l
  dpy <- asks display
  xmf <- initXMF $ hintFont h
  wm <- windowMap $ hintChar h
  hs <- createHints xmf titleLen wm
  let ws = map (\(x, y) -> y) hs
      ks = map (\(x, y) -> x) wm
  showWindows ws
  drawHints dpy xmf fg bg hs
  mkXPrompt t c (\s -> return $ filter (isPrefixOf s) ks) $ \s -> do
    let mw = lookup s wm
    case mw of
      Nothing -> return ()
      Just w -> hintAction t w
  deleteWindows ws
  releaseXMF xmf
  storeLayout currentLayout

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
      str = " " ++ map toUpper str' ++ ": " ++ str''' ++ " "
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

dumpLayout :: Maybe (Layout Window) -> X (Maybe (Layout Window))
dumpLayout Nothing = return Nothing
dumpLayout (Just l) =
  do state <- gets windowset
     let currentLayout = W.layout . W.workspace . W.current $ state
     setLayout l
     return $ Just currentLayout

storeLayout :: Maybe (Layout Window) -> X ()
storeLayout Nothing = return ()
storeLayout (Just l) = setLayout l
