import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import Data.List
import XMonad.Hints

myMask = modMask defaultConfig
myWorkspaces = map (\x -> [x]) ['a'..'z']
myTall = Tall 1 (3/100) (5/7)
myXPConfig = defaultXPConfig
  { searchPredicate = isInfixOf
  }
hintChar = map (\x -> [x]) "jfnvuthgybkdmciroelspwaqz"

-- Hint functions
swapMasterWith :: (Eq s, Eq a, Eq i) =>
  a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swapMasterWith a = W.swapMaster . W.focusWindow a

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , borderWidth = 0
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ (Mirror myTall ||| myTall ||| Full)
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
    , startupHook = spawn $ "xscreensaver -no-splash"
      ++ "& unclutter -idle 1 -jitter 100 -root"
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
      | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall")]
    ] ++
    [ ("M-p " ++ key, action)
    | (key, action) <- [("g", windowPromptGoto myXPConfig), ("b", windowPromptBring myXPConfig)]
    ] ++
    [ ("M-f", runHints focus hintChar) ] ++
    [ ("M-; " ++ key, runHints action hintChar)
    | (key, action) <- [ ("f", focus), ("c", killWindow), ("m", windows . swapMasterWith) ] ++
        [ ("u " ++ otherModMasks ++ tag, action tag)
        | tag <- myWorkspaces
        , (otherModMasks, action) <- [ ("S-", (\t -> windows . W.shiftWin t))
                                     , ("", (\t a -> windows $ W.greedyView t . W.shiftWin t a))]
        ]
    ]
    )

