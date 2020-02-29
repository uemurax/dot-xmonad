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
import XMonad
import qualified XMonad.StackSet as W
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

-- User libraries
import XMonad.Prompt.MyHints ( HintConfig (..)
                             , HintPrompt (..)
                             , hintPrompt )
import XMonad.Util.MyList ( enumWords )
import XMonad.Layout.MyCircle ( MyCircle (..) )
import XMonad.Config.MyFullscreen ( myFullscreen )
import XMonad.Config.MyDocks ( myDocks )
import XMonad.Config.MyMaximize ( myMaximize )
import XMonad.Config.MyLayoutBase ( myLayoutBase )

-- Main configuration
myConfig = myFullscreen . myDocks . myMaximize . myLayoutBase $ myConfigBase

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
  , hintLayout = return . Just . Layout $ MyCircle 0 (3/100) (4/7) (1/11)
  }

myHintPrompt action = hintPrompt action myHConfig $ myXPConfig
                      { autoComplete = Just 0
                      , searchPredicate = isPrefixOf
                      }

myConfigBase =
  def { borderWidth = 0
      , workspaces = take 64 $ enumWords ['a'..'z']
      , focusFollowsMouse = False
      , clickJustFocuses = True
      } `additionalKeysP`
  [ ("M-f", myHintPrompt Focus)
  , ("M-n", nextWS)
  , ("M-p", prevWS)
  , ("M-S-n", shiftToNext)
  , ("M-S-p", shiftToPrev)
  , ("M-t", withFocused $ windows . W.sink)
  ]
