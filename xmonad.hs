import XMonad hiding ( (|||) )
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.CycleWS
import System.IO
import Data.List
import Data.Default
import qualified Data.Map as M
import Control.Arrow (first)
import XMonad.Prompt.Hints
import XMonad.Util.List
import XMonad.Util.Misc
import XMonad.Actions.Transset
import XMonad.Layout.CircleEX
import XMonad.Layout.ExpandFocus

myConfig = def
myMask = modMask myConfig
numWorkspaces = 64
myWorkspaces =  take numWorkspaces $ enumWords ['a'..'z']
mySpacing = expandFocus 10 . spacing 10
myTall = renamed [Replace "Tall"] . mySpacing $ Tall 1 (3/100) (5/7)
myCircle = CircleEX 1 (3/100) (5/7)
myFont = "xft:IPAGothic"
myXPConfig = def
  { searchPredicate = isInfixOf
  , font = myFont
  , promptKeymap = M.unionWith (\x y -> y) emacsLikeXPKeymap $
                   M.fromList $
                   map (first $ (,) controlMask)
                   [ (xK_h, deleteString Prev)
                   , (xK_u, killBefore)
                   , (xK_w, killWord Prev)
                   ]
  }
myHConfig = def
  { hintFont = myFont
  }
myTranssetConfig = def

myHintPrompt action = hintPrompt action myHConfig $ myXPConfig
                      { autoComplete = Just 0
                      , searchPredicate = isPrefixOf
                      }

main = do
  xmonad $ myConfig
    { terminal = "x-terminal-emulator"
    , borderWidth = 0
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook myConfig
    , layoutHook = avoidStruts $ (Mirror myTall ||| myTall ||| Full ||| myCircle ||| Mirror myCircle)
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
      | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "CircleEX"), ("S-c", "Mirror CircleEX")]
    ] ++
    [ ("M-: " ++ key, action)
    | (key, action) <- [("g", windowPromptGoto myXPConfig), ("b", windowPromptBring myXPConfig)]
    ] ++
    [ ("M-f", myHintPrompt Focus) ] ++
    [ ("M-; " ++ key, myHintPrompt action)
    | (key, action) <- [ ("f", Focus), ("m", BringToMaster)
        , ("c", Close), ("s", Swap)
        , ("t", Sink), ("S-t", Float), ("S-m", Maximize)
        , ("@", ScreenShot $ \w -> return $ "~/screenshot-" ++ show w ++ ".png")
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

