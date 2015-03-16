module XMonad.Hints
( runHints
, defaultHConfig
) where

import XMonad hiding (initColor)
import qualified XMonad.StackSet as W

data HintConfig = HintConfig
  { hintChar :: [String]
  , font :: String
  , fgColor :: String
  , bgColor :: String
  }

defaultHConfig :: HintConfig
defaultHConfig = HintConfig
  { hintChar = map (\x -> [x]) "jfnvuthgybkdmciroelspwaqz"
  , font = "fixed"
  , fgColor = "#101000"
  , bgColor = "#ffdfff"
  }

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

printString :: Display
  -> Drawable
  -> GC
  -> FontStruct
  -> Pixel                -- fgColor
  -> Pixel                -- bgColor
  -> String               -- text
  -> IO ()
printString dpy d gc fontst fgcolor bgcolor str = do
  let strLen = textWidth fontst str
      valign = strLen * 2
      remWidth = 20 - strLen
      offset = remWidth `div` 2
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  drawImageString dpy d gc offset valign str

currentWorkspace :: W.StackSet i l a s sd -> W.Workspace i l a
currentWorkspace = W.workspace . W.current

currentWindows :: W.StackSet i l a s sd -> [a]
currentWindows = W.integrate' . W.stack . currentWorkspace

createPanel :: Display
  -> FontStruct
  -> Pixel             -- bgColor
  -> Window
  -> IO Window
createPanel dpy fnt background win = do
  let dflt = defaultScreen dpy
      strLen = textWidth fnt "a"
      height = fromIntegral strLen * 3
      width = fromIntegral strLen * 4
  (rootw, x, y, w, h, b, d) <- getGeometry dpy win
  let dx = (w - width) `div` 2
      dy = (h - height) `div` 2
      x' = x + fromIntegral dx
      y' = y + fromIntegral dy
  pnl <- createSimpleWindow dpy rootw x' y' width height 0 0 background
  return pnl

drawPanel :: Display
  -> Window
  -> FontStruct
  -> Pixel              -- fgColor
  -> Pixel              -- bgColor
  -> String             -- text
  -> IO ()
drawPanel dpy pnl fontStruc fg bg str = do
  gc <- createGC dpy pnl
  printString dpy pnl gc fontStruc fg bg str
  freeGC dpy gc

drawPanels :: Display
  -> FontStruct
  -> Pixel             -- fgColor
  -> Pixel             -- bgColor
  -> [(String, Window)]
  -> IO ()
drawPanels dpy fnt fg bg [] = return ()
drawPanels dpy fnt fg bg ((s, p):ps) = do
  drawPanel dpy p fnt fg bg s
  drawPanels dpy fnt fg bg ps

createPanels :: Display
  -> FontStruct
  -> Pixel             -- bgColor
  -> [Window]
  -> IO [Window]
createPanels dpy fnt bg [] = return []
createPanels dpy fnt bg (w:ws) = do
  p <- createPanel dpy fnt bg w
  ps <- createPanels dpy fnt bg ws
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

waitKey :: Display -> IO String
waitKey dpy = do
  allocaXEvent $ keyEventHandler dpy

keyEventHandler :: Display -> XEventPtr -> IO String
keyEventHandler dpy e = do
  nextEvent dpy e
  tp <- get_EventType e
  if tp == keyPress then do
    (_, k) <- lookupString $ asKeyEvent e
    return k
  else keyEventHandler dpy e

followHint :: Display
  -> FontStruct
  -> Pixel               -- fgColor
  -> Pixel               -- bgColor
  -> [(String, Window)]
  -> IO (Maybe Window)
followHint dpy fnt fg bg ws = do
  let (strs, wins) = (map fst ws, map snd ws)
  ps <- createPanels dpy fnt bg wins
  mapPanels dpy ps
  drawPanels dpy fnt fg bg $ zip strs ps
  rootw <- rootWindow dpy (defaultScreen dpy)
  dammy <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  mapWindow dpy dammy
  selectInput dpy dammy keyPressMask
  setInputFocus dpy dammy revertToPointerRoot 0
  sync dpy False
  key <- waitKey dpy
  destroyWindow dpy dammy
  destroyPanels dpy ps
  return $ lookup key ws

runHints :: HintConfig -> (Window -> X ()) -> X ()
runHints config action = withDisplay $ \dpy ->
  withWindowSet $ \stk ->
    withFocused $ \orgWin -> do
      let strs = hintChar config
      fnt <- io . loadQueryFont dpy $ font config
      fg <- io . initColor dpy $ fgColor config
      bg <- io . initColor dpy $ bgColor config
      newWin <- io . followHint dpy fnt fg bg . zip strs . currentWindows $ stk
      case newWin of
        Nothing -> focus orgWin
        Just win -> action win
      refresh
      io $ freeFont dpy fnt

