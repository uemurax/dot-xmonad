import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
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
import XMonad.Actions.Transset

myConfig = def
myMask = modMask myConfig
numWorkspaces = 64
myWorkspaces =  take numWorkspaces $ enumWords ['a'..'z']
myTall = Tall 1 (3/100) (5/7)
myXPConfig = def
  { searchPredicate = isInfixOf
  }
myHConfig = def
  { hintFont = "xft:MigMix 1P"
  }
myTranssetConfig = def

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ myConfig
    { terminal = "x-terminal-emulator"
    , borderWidth = 0
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook myConfig
    , layoutHook = avoidStruts $ (Mirror myTall ||| myTall ||| Full ||| Circle ||| Mirror Circle)
    , logHook = do
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
          }
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
    | (key, command) <- [ ("e", "x-terminal-emulator -e emacsclient -t")
        , ("w", "x-www-browser")
        ]
    ] ++
    [ ("M-" ++ key, withFocused $ runTransset myTranssetConfig t)
    | (key, t) <- [("S-.", Inc), (">", Inc), ("S-,", Dec), ("<", Dec), ("S-o", Toggle)]
    ]
    )

