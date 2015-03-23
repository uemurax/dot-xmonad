module XMonad.Hints
( runHints
, defaultHConfig
) where

import XMonad hiding (initColor)
import qualified XMonad.StackSet as W
import Foreign.C.String

data HintConfig = HintConfig
  { hintChar :: [String]
  , hintFont :: String
  , hintFgColor :: String
  , hintBgColor :: String
  , hintBorderWidth :: Int
  , hintBorderColor :: String
  -- If `hintTitleMaxLen` is negative then
  -- show full title in hint
  , hintTitleMaxLen :: Int
  }

defaultHConfig :: HintConfig
defaultHConfig = HintConfig
  { hintChar = map (\x -> [x]) "jfnvuthgybkdmciroelspwaqz"
  , hintFont = "fixed"
  , hintFgColor = "#000000"
  , hintBgColor = "#ffff00"
  , hintBorderWidth = 1
  , hintBorderColor = "#101010"
  , hintTitleMaxLen = 30
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
  -> Position
  -> Position
  -> String               -- text
  -> IO ()
printString dpy d gc fontst fgcolor bgcolor offset valign str = do
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  drawImageString dpy d gc offset valign str

currentWorkspace :: W.StackSet i l a s sd -> W.Workspace i l a
currentWorkspace = W.workspace . W.current

currentWindows :: W.StackSet i l a s sd -> [a]
currentWindows = W.integrate' . W.stack . currentWorkspace

createPanel :: Display
  -> FontStruct
  -> Int               -- borderWidth
  -> Pixel             -- borderColor
  -> Pixel             -- fgColor
  -> Pixel             -- bgColor
  -> Int               -- titleMaxLen
  -> (String, Window)
  -> IO Window
createPanel dpy fnt bdw bdc fgColor background titleLen (str, win) = do
  textProp <- getTextProperty dpy win wM_NAME
  wName <- peekCString . tp_value $ textProp
  let str'' = if titleLen < 0 then wName
              else take titleLen wName
      str' = str ++ ": " ++ str''
  let dflt = defaultScreen dpy
      chLen = textWidth fnt "w"
      strLen = textWidth fnt str'
      height = fromIntegral chLen * 3
      width = fromIntegral (strLen + chLen * 2)
  (rootw, x, y, w, h, b, d) <- getGeometry dpy win
  let dx = (w - width) `div` 2
      dy = (h - height) `div` 2
      x' = x + fromIntegral dx
      y' = y + fromIntegral dy
  pnl <- createSimpleWindow dpy rootw x' y' width height (fromIntegral bdw) bdc background
  mapWindow dpy pnl
  drawPanel dpy pnl fnt fgColor background chLen (chLen * 2) str'
  return pnl

drawPanel :: Display
  -> Window
  -> FontStruct
  -> Pixel              -- fgColor
  -> Pixel              -- bgColor
  -> Position
  -> Position
  -> String             -- text
  -> IO ()
drawPanel dpy pnl fontStruc fg bg offset valign str = do
  gc <- createGC dpy pnl
  printString dpy pnl gc fontStruc fg bg offset valign str
  freeGC dpy gc

raiseWindows :: Display -> [Window] -> IO ()
raiseWindows dpy [] = return ()
raiseWindows dpy (w:ws) = do
  raiseWindow dpy w
  raiseWindows dpy ws

createPanels :: Display
  -> FontStruct
  -> Int               -- borderWidth
  -> Pixel             -- borderColor
  -> Pixel             -- fgColor
  -> Pixel             -- bgColor
  -> Int               -- titleLen
  -> [(String, Window)]
  -> IO [Window]
createPanels dpy fnt bdw bdc fg bg showTitle [] = return []
createPanels dpy fnt bdw bdc fg bg showTitle (w:ws) = do
  p <- createPanel dpy fnt bdw bdc fg bg showTitle w
  ps <- createPanels dpy fnt bdw bdc fg bg showTitle ws
  return (p:ps)

destroyPanels :: Display -> [Window] -> IO ()
destroyPanels dpy [] = return ()
destroyPanels dpy (p:ps) = do
  destroyWindow dpy p
  destroyPanels dpy ps

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
  -> Int                 -- borderWidth
  -> Pixel               -- borderColor
  -> Pixel               -- fgColor
  -> Pixel               -- bgColor
  -> Int                 -- titleLen
  -> [String]
  -> [Window]
  -> IO (Maybe Window)
followHint dpy fnt bdw bdc fg bg showTitle strs wins = do
  let ws = zip strs wins
  raiseWindows dpy wins
  ps <- createPanels dpy fnt bdw bdc fg bg showTitle ws
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
          bdw = hintBorderWidth config
          titleLen = hintTitleMaxLen config
      fnt <- io . loadQueryFont dpy $ hintFont config
      bdc <- io . initColor dpy $ hintBorderColor config
      fg <- io . initColor dpy $ hintFgColor config
      bg <- io . initColor dpy $ hintBgColor config
      newWin <- io . followHint dpy fnt bdw bdc fg bg titleLen strs $ currentWindows stk
      case newWin of
        Nothing -> focus orgWin
        Just win -> action win
      refresh
      io $ freeFont dpy fnt

