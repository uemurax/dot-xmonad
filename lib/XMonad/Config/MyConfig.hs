{-# LANGUAGE FlexibleContexts #-}

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
                     , killWord
                     )
import XMonad.Util.EZConfig( additionalKeysP
                           )
import XMonad.Actions.CycleWS ( nextWS
                              , prevWS
                              , shiftToNext
                              , shiftToPrev
                              )
import XMonad.Layout.Fullscreen ( fullscreenFocus
                                , fullscreenManageHook
                                , fullscreenEventHook
                                )
import XMonad.Hooks.ManageDocks ( docks
                                , avoidStruts
                                )

-- User libraries
import XMonad.Prompt.MyHints ( HintConfig (..)
                             , HintPrompt (..)
                             , hintPrompt )
import XMonad.Util.MyList ( enumWords )
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

myHintPrompt action = hintPrompt action myHConfig $ myXPConfig
                      { autoComplete = Just 0
                      , searchPredicate = isPrefixOf
                      }

myConfig = myFullscreen . myDocks $ myConfigBase

myFullscreen cfg =
  cfg { handleEventHook = handleEventHook cfg <+> fullscreenEventHook
      , manageHook =  manageHook cfg <+> fullscreenManageHook
      , layoutHook = fullscreenFocus  $ layoutHook cfg
      }

myDocks cfg = docks cfg { layoutHook = avoidStruts $ layoutHook cfg }

myConfigBase =
  def { borderWidth = 0
      , workspaces = take 64 $ enumWords ['a'..'z']
      , layoutHook = (myTall ||| Mirror myTall ||| Full ||| myCircle ||| Mirror myCircle)
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
  [ ("M-t", withFocused $ windows . W.sink)
  ]
  )
