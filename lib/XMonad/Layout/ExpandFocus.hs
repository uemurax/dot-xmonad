{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.ExpandFocus
       ( expandFocus
       ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier

expandFocus :: Int -> l a -> ModifiedLayout ExpandFocus l a
expandFocus n = ModifiedLayout (ExpandFocus n)

data ExpandFocus a = ExpandFocus Int deriving (Show, Read)

instance (Eq a) => LayoutModifier ExpandFocus a where
  pureModifier _ r Nothing wrs = (wrs, Nothing)
  pureModifier (ExpandFocus n) r (Just stk) wrs = (modifyFocusRect (expandRect n) stk wrs, Nothing)

  modifierDescription (ExpandFocus n) = "Expand Focus " ++ show n

modifyFocusRect :: (Eq a) => (Rectangle -> Rectangle) -> W.Stack a -> [(a, Rectangle)] -> [(a, Rectangle)]
modifyFocusRect f (W.Stack a _ _) wrs = map (\(w, r) -> (w, if a == w then f r else r)) wrs

expandRect :: Int -> Rectangle -> Rectangle
expandRect n (Rectangle x y w h) = Rectangle (x - fromIntegral n) (y - fromIntegral n) (w + 2 * fromIntegral n) (h + 2 * fromIntegral n)
