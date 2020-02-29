module XMonad.Config.MyLayoutBase
  ( myLayoutBase
  ) where

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||)
                                       , JumpToLayout (JumpToLayout) )
import XMonad.Util.EZConfig
import XMonad.Layout.MyCircle ( MyCircle (..) )

myLayoutBase cfg =
  cfg { layoutHook = myLayout
      } `additionalKeysP`
  myLayoutKeysP

myLayout = myTall ||| Mirror myTall ||| Full ||| myCircle ||| Mirror myCircle

myTall = Tall 1 (3/100) (4/7)
myCircle = MyCircle 1 (3/100) (4/7) (1/11)

myLayoutKeysP =
  [ ("M-o " ++ key, sendMessage $ JumpToLayout layout)
  | (key, layout) <- [("f", "Full"), ("t", "Tall"), ("S-t", "Mirror Tall"), ("c", "MyCircle"), ("S-c", "Mirror MyCircle")]
  ]
