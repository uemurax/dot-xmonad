{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.CircleEX (
  CircleEX (..)
  ) where

import Data.List
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W

data CircleEX a = CircleEX {
  circleNMaster :: !Int,
  circleRatioIncrement :: !Rational,
  circleRatio :: !Rational }
                  deriving (Show, Read)

instance LayoutClass CircleEX Window where
  doLayout (CircleEX nmaster _ frac) r s = do
    layout <- raiseFocus $ zip ws rs
    return (layout, Nothing) where
      ws = W.integrate s
      rs = circleLayout frac r nmaster (length ws)

  pureMessage (CircleEX nmaster delta frac) m =
    msum [ fmap resize (fromMessage m),
           fmap incmastern (fromMessage m)] where
      resize Shrink = CircleEX nmaster delta (max 0 $ frac - delta)
      resize Expand = CircleEX nmaster delta (min 1 $ frac + delta)
      incmastern (IncMasterN d) = CircleEX (max 0 (nmaster + d)) delta frac

  description _ = "CircleEX"

raiseFocus :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
raiseFocus xs = do
  focused <- withWindowSet (return . W.peek)
  return $ case find ((== focused) . Just . fst) xs of
            Just x -> x : delete x xs
            Nothing -> xs

circleLayout :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
circleLayout frac r nmaster n = center ++ rest where
  nmaster' = if nmaster < 0 then 0
             else if nmaster > n then n
                  else nmaster
  center = centerRect frac r nmaster'
  rest = satellite (1 - frac) r (n - nmaster')

centerRect :: Rational -> Rectangle -> Int -> [Rectangle]
centerRect _ _ n | n < 1 = []
centerRect frac (Rectangle x y w h) n =
  splitVertically n (Rectangle x' y' (floor w') (floor h')) where
    x' = x + floor ((fromIntegral w - w') / 2)
    y' = y + floor ((fromIntegral h - h') / 2)
    w' = fromIntegral w * frac
    h' = fromIntegral h * frac

satellite :: Rational -> Rectangle -> Int -> [Rectangle]
satellite frac (Rectangle x y w h) n =
  map f [0, 2 * pi / fromIntegral n ..] where
    f t = Rectangle x' y' (floor w') (floor h') where
      x' = x + round ((fromIntegral w - w') * (1.0 + cos t) / 2)
      y' = y + round ((fromIntegral h - h') * (1.0 + sin t) / 2)
      w' = fromRational (fromIntegral w * frac)
      h' = fromRational (fromIntegral h * frac)
