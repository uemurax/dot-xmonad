module XMonad.Config.MyConfig (
  myConfig
  ) where

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

-- User libraries
import XMonad.Prompt.MyHints ( HintConfig (..)
                             , HintPrompt (..)
                             , hintPrompt )
import XMonad.Util.MyList ( enumWords )
import XMonad.Util.MyMisc ( maximizeWindow )
import XMonad.Actions.MyTransset ( Transset (..)
                               , runTransset )
import XMonad.Actions.MyXdotool ( Xdotool (..)
                              , runXdotool )
import XMonad.Layout.MyCircle ( MyCircle (..) )

-- Main configuration
myTall = Tall 1 (3/100) (4/7)
myCircle = MyCircle 1 (3/100) (4/7) (1 / 11)
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
           then return . Just . Layout $ MyCircle 0 (3/100) (4/7) (1/11)
           else return Nothing
  }
myTranssetConfig = def

myHintPrompt action = hintPrompt action myHConfig $ myXPConfig
                      { autoComplete = Just 0
                      , searchPredicate = isPrefixOf
                      }

myConfig = myConfigBase

myConfigBase =
  def {  borderWidth = 0
      , workspaces = take 64 $ enumWords ['a'..'z']
      , layoutHook = (Mirror myTall ||| myTall ||| Full ||| myCircle ||| Mirror myCircle)
      , focusFollowsMouse = False
      , clickJustFocuses = True
      } `additionalKeysP` (
  [ ("M-o " ++ key, sendMessage $ JumpToLayout layout)
  | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "MyCircle"), ("S-c", "Mirror MyCircle")]
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
  ]
  )
