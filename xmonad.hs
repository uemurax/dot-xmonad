import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Circle
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.CycleWS
import System.IO
import Data.List
import XMonad.Hints

myMask = modMask defaultConfig
myWorkspaces = map (\x -> [x]) ['a'..'z']
myTall = Tall 1 (3/100) (5/7)
myXPConfig = defaultXPConfig
  { searchPredicate = isInfixOf
  }
myHConfig = defaultHConfig
myFadeHook = composeAll
  [ isUnfocused --> transparency 0.2
  ]

-- $Hint functions
swapMasterWith :: (Eq s, Eq a, Eq i) =>
  a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swapMasterWith a = W.swapMaster . W.focusWindow a

-- | swap the current focused window and the specified window and focus the specified window
swapWith :: (Eq a) =>
  a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swapWith = W.modify' . swapWith'

swapWith' :: (Eq a) =>
  a -> W.Stack a -> W.Stack a
swapWith' a stk@(W.Stack t ls rs) =
  if a == t then stk
  else case findWindow a ls rs of
    Nothing -> stk
    Just (Left (ll, lr)) -> W.Stack t ll (lr ++ [a] ++ rs)
    Just (Right (rl, rr)) -> W.Stack t (rr ++ [a] ++ ls) rl

findWindow :: (Eq a) => a -> [a] -> [a] -> Maybe (Either ([a], [a]) ([a], [a]))
findWindow a ls rs = case findZ a (ls, []) of
  Nothing -> case findZ a (rs, []) of
    Nothing -> Nothing
    Just r -> Just (Right r)
  Just l -> Just (Left l)

findZ :: (Eq a) => a -> ([a], [a]) -> Maybe ([a], [a])
findZ a ([], _) = Nothing
findZ a (x:xs, ys) =
  if a == x then Just (xs, ys)
  else findZ a (xs, x:ys)


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , borderWidth = 0
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ (Mirror myTall ||| myTall ||| Full ||| Circle)
    , logHook = do
        fadeWindowsLogHook myFadeHook
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
          }
    , handleEventHook = fadeWindowsEventHook
    , startupHook = spawn $ "xscreensaver -no-splash"
      ++ "& unclutter -idle 1 -jitter 100 -root"
      ++ "& xcompmgr"
      ++ "& feh --bg-scale ~/Pictures/desktop-background"
    , focusFollowsMouse = False
    , clickJustFocuses = True
    } `additionalKeysP` (
    [ ("M-S-z" , spawn "xscreensaver-command -lock")
    ] ++
    [ ("M-u " ++ otherModMasks ++ tag, action tag)
      | tag <- myWorkspaces
      , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                   , ("S-", windows . W.shift)]
    ] ++
    [ ("M-o " ++ key, sendMessage $ JumpToLayout layout)
      | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "Circle")]
    ] ++
    [ ("M-: " ++ key, action)
    | (key, action) <- [("g", windowPromptGoto myXPConfig), ("b", windowPromptBring myXPConfig)]
    ] ++
    [ ("M-f", runHints myHConfig focus) ] ++
    [ ("M-; " ++ key, runHints myHConfig action)
    | (key, action) <- [ ("f", focus), ("c", killWindow)
        , ("m", windows . swapMasterWith), ("s", windows . swapWith) ] ++
        [ ("u " ++ otherModMasks ++ tag, action tag)
        | tag <- myWorkspaces
        , (otherModMasks, action) <- [ ("S-", (\t -> windows . W.shiftWin t))
                                     , ("", (\t a -> windows $ W.greedyView t . W.shiftWin t a))]
        ]
    ] ++
    [ ("M-n", nextWS), ("M-p", prevWS)
    , ("M-S-n", shiftToNext), ("M-S-p", shiftToPrev)
    ]
    )

