import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Circle
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.CycleWS
import System.IO
import Data.List
import Data.Default
import XMonad.Prompt.Hints
import XMonad.Util.List
import XMonad.Util.Misc

myConfig = def
myMask = modMask myConfig
numWorkspaces = 64
myWorkspaces =  take numWorkspaces $ enumWords ['a'..'z']
myTall = Tall 1 (3/100) (5/7)
myXPConfig = def
  { searchPredicate = isInfixOf
  }
myHConfig = def
myFadeHook = composeAll
  [ transparency 0.25
  , isFloating --> transparency 0.5
  ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ myConfig
    { terminal = "urxvt"
    , borderWidth = 0
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook myConfig
    , layoutHook = avoidStruts $ (Mirror myTall ||| myTall ||| Full ||| Circle ||| Mirror Circle)
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
    [ ("M-u " ++ key, workspacePrompt myXPConfig action)
    | (key, action) <- [ ("g", windows . W.greedyView)
                       , ("b", windows . W.shift)
                       , ("f", \s -> hintPrompt (BringToWS s) myHConfig myXPConfig)
                       ]
    ] ++
    [ ("M-o " ++ key, sendMessage $ JumpToLayout layout)
      | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "Circle"), ("S-c", "Mirror Circle")]
    ] ++
    [ ("M-: " ++ key, action)
    | (key, action) <- [("g", windowPromptGoto myXPConfig), ("b", windowPromptBring myXPConfig)]
    ] ++
    [ ("M-f", hintPrompt Focus myHConfig myXPConfig) ] ++
    [ ("M-; " ++ key, hintPrompt action myHConfig myXPConfig)
    | (key, action) <- [ ("f", Focus), ("m", BringToMaster)
        , ("c", Close), ("s", Swap)
        , ("t", Sink), ("S-t", Float), ("S-m", Maximize)
        ]
    ] ++
    [ ("M-n", nextWS), ("M-p", prevWS)
    , ("M-S-n", shiftToNext), ("M-S-p", shiftToPrev)
    ] ++
    [ ("M-S-t", withFocused maximizeWindow)
    , ("M-t", withFocused $ windows . W.sink)
    ] ++
    [ ("M-@", spawn "import -window root screenshot.jpg")
    ] ++
    [ ("M-a " ++ key, spawn command)
    | (key, command) <- [ ("e", "urxvt -e emacsclient -t")
        , ("w", "dwb")
        ]
    ]
    )

