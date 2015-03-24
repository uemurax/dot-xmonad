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
import XMonad.Prompt.Hints

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
    [ ("M-f", hintPrompt Focus defaultHConfig myXPConfig) ] ++
    [ ("M-; " ++ key, hintPrompt action defaultHConfig myXPConfig)
    | (key, action) <- [ ("f", Focus), ("m", BringToMaster)
        , ("c", Close), ("s", Swap)
        , ("t", Sink), ("S-t", Float)
        ] ++
        [ ("u " ++ tag, BringToWS tag)
        | tag <- myWorkspaces
        ]
    ] ++
    [ ("M-n", nextWS), ("M-p", prevWS)
    , ("M-S-n", shiftToNext), ("M-S-p", shiftToPrev)
    ] ++
    [ ("M-S-t", withFocused float)
    , ("M-t", withFocused $ windows . W.sink)
    ]
    )

