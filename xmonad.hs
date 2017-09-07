-- System libraries
import System.IO
import Data.List
import Data.Default
import qualified Data.Map as M
import Control.Arrow ( first )

-- XMonad libraries
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators ( (|||)
                                       , JumpToLayout (JumpToLayout) )
import XMonad.Layout.Spacing ( spacing )
import XMonad.Layout.Renamed ( renamed
                             , Rename (..) )
import XMonad.Prompt ( XPConfig (..)
                     , Direction1D (Prev)
                     , emacsLikeXPKeymap
                     , deleteString
                     , killBefore
                     , killWord )
import XMonad.Util.Run( spawnPipe )
import XMonad.Util.EZConfig( additionalKeysP
                           , additionalKeys )
import XMonad.Actions.CycleWS ( nextWS
                              , prevWS
                              , shiftToNext
                              , shiftToPrev )
import XMonad.Actions.KeyRemap ( KeymapTable (..)
                               , buildKeyRemapBindings
                               , setDefaultKeyRemap
                               , setKeyRemap
                               , emptyKeyRemap )

-- User libraries
import XMonad.Prompt.Hints ( HintConfig (..)
                           , HintPrompt (..)
                           , hintPrompt )
import XMonad.Util.List ( enumWords )
import XMonad.Util.Misc ( maximizeWindow )
import XMonad.Actions.Transset ( Transset (..)
                               , runTransset )
import XMonad.Actions.Xdotool ( Xdotool (..)
                              , runXdotool )
import XMonad.Layout.CircleEX ( CircleEX (..) )

-- Main configuration
myConfig = def
myTall = renamed [Replace "Tall"] . spacing 2 $ Tall 1 (3/100) (4/7)
myCircle = CircleEX 1 (3/100) (4/7) (1 / 11)
myFont = "xft:monospace:size=12"
myHintFont = "xft:monospace:size=16"
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
  { hintFont = myHintFont
  , hintLayout =
      do l <- withWindowSet $ return . W.layout . W.workspace . W.current
         let s = description l
         if "Full" `isInfixOf` s
           then return . Just . Layout $ CircleEX 0 (3/100) (4/7) (1/11)
           else return Nothing
  }
myTranssetConfig = def

myHintPrompt action = hintPrompt action myHConfig $ myXPConfig
                      { autoComplete = Just 0
                      , searchPredicate = isPrefixOf
                      }

myKeyRemap = KeymapTable [ ((controlMask, xK_i), (0, xK_Tab))
                         , ((controlMask, xK_m), (0, xK_Return))
                         , ((controlMask, xK_bracketleft), (0, xK_Escape))
                         ]

main = do
  xmonad $ myConfig
    { terminal = "urxvtc -e tmux a"
    , borderWidth = 0
    , workspaces = take 64 $ enumWords ['a'..'z']
    , layoutHook = (Mirror myTall ||| myTall ||| Full ||| myCircle ||| Mirror myCircle)
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , startupHook = do
        setDefaultKeyRemap emptyKeyRemap [emptyKeyRemap, myKeyRemap]
    } `additionalKeysP` (
    [ ("M-z" , spawn "slock")
    ] ++
    [ ("M-o " ++ key, sendMessage $ JumpToLayout layout)
      | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "CircleEX"), ("S-c", "Mirror CircleEX")]
    ] ++
    [ ("M-f", myHintPrompt Focus) ] ++
    [ ("M-n", nextWS), ("M-p", prevWS)
    , ("M-S-n", shiftToNext), ("M-S-p", shiftToPrev)
    ] ++
    [ ("M-S-t", withFocused maximizeWindow)
    , ("M-t", withFocused $ windows . W.sink)
    ] ++
    [ ("M-@", spawn "import -window root screenshot.jpg")
    ] ++
    [ ("M-a " ++ key, spawn command)
    | (key, command) <- [ ("e", "emacsclient -c")
                        , ("t", "urxvtc -e tmux a")
                        , ("w", "x-www-browser")
                        , ("m", "urxvtc -e ncmpcpp")
                        ]
    ] ++
    [ ("M-" ++ key, withFocused $ runTransset myTranssetConfig t)
    | (key, t) <- [("S-.", Inc), (">", Inc), ("S-,", Dec), ("<", Dec), ("S-o", Toggle)]
    ] ++
    [ ("M4-" ++ key, runXdotool t)
    | (key, t) <- [ ("h", MousemoveRelative (-10) 0)
                  , ("j", MousemoveRelative 0 10)
                  , ("k", MousemoveRelative 0 (-10))
                  , ("l", MousemoveRelative 10 0)
                  , ("m", Click 1)
                  , ("w", Click 1)
                  , ("e", Click 2)
                  , ("r", Click 3)
                  , ("n", Click 5)
                  , ("p", Click 4)
                  , ("d", Mousedown 1)
                  , ("u", Mouseup 1)
                  ]
    ] ++
    [ ("M-i " ++ key, setKeyRemap t)
    | (key, t) <- [("i", emptyKeyRemap), ("o", myKeyRemap)]
    ]
    ) `additionalKeys` buildKeyRemapBindings
    [ myKeyRemap
    ]
