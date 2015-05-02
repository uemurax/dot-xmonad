module XMonad.Actions.Transset
       ( Transset (..)
       , runTransset
       ) where

import XMonad

import Data.Default

data Transset = Inc | Dec | Toggle

runTransset :: TranssetConfig -> Transset -> Window -> X ()
runTransset c t w = spawn $ "transset -i " ++ show w ++ " " ++ f t
  where f Inc = "--inc " ++ show (transIncrement c)
        f Dec = "--dec " ++ show (transIncrement c)
        f Toggle = "--toggle"

data TranssetConfig = TranssetConfig
  { transIncrement :: Float
  }

instance Default TranssetConfig where
  def = TranssetConfig
        { transIncrement = 0.05
        }

