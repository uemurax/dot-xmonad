module XMonad.Prompt.Hints
  ( hintsPromptFocus
  , defaultHConfig
  ) where

import qualified Data.Map as M

import XMonad
import XMonad.Prompt
import XMonad.Util.Hints
import XMonad.Util.XUtils

data HintConfig = HintConfig
  { hintChar :: String
  }

defaultHConfig = HintConfig
  { hintChar = "jfnvuthgbymridkcoelspwaqz"
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
  wm <- windowMap $ hintChar h
  ws <- createHints wm
  showWindows ws
  a <- case t of
    Focus -> return $ focusAction wm
  mkXPrompt t c (\_ -> return []) a
  deleteWindows ws

focusAction :: M.Map String Window -> String -> X ()
focusAction wm s = do
  let mw = M.lookup s wm
  case mw of
    Nothing -> return ()
    Just w -> focus w

createHints :: M.Map String Window -> X ([Window])
createHints wm = createHints' $ M.toList wm

createHints' :: [(String, Window)] -> X ([Window])
createHints' [] = return []
createHints' (w:ws) = do
  p <- createHint w
  ps <- createHints' ws
  return (p:ps)

createHint :: (String, Window) -> X Window
createHint (str, win) = do
  dpy <- asks display
  (rootw, x, y, w, h, b, d) <- io $ getGeometry dpy win
  ret <- createNewWindow (Rectangle x y 10 10) Nothing "" False
  return ret

