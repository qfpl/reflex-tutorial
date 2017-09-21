{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Colour where

import Data.Monoid ((<>))

import qualified Data.Text as Text

import Reflex.Dom.Core

import Util.Reflex
import Util.Grid.Config
import Util.Grid.Square
import Util.SVG

data Colour =
    Red
  | Blue
  deriving (Eq, Ord, Show)

flipColour :: Colour
          -> Colour
flipColour Red =
  Blue
flipColour Blue =
  Red

instance Square Colour where
  mkSquareAttrs gc y x c =
    let
      mkFill (Just Red) =
        "fill" =: "red" <>
        "class" =: "grid-square"
      mkFill (Just Blue) =
        "fill" =: "blue" <>
        "class" =: "grid-square"
      mkFill Nothing =
        "fill" =: "none" <>
        "stroke" =: "none"
    in
      (mkFill <$> c) <> pure (standardAttrs gc y x)

instance Square (Colour, Colour) where
  mkSquareAttrs _ _ _ _ =
    pure mempty
  mkSquare gc y x dPair =
    let
      s =
        Text.pack . show $ _gcSquareSize gc
      sx1 =
        Text.pack . show $ _gcSquareSize gc * x
      sx2 =
        Text.pack . show $ _gcSquareSize gc * x + _gcSquareSize gc `div` 2
      sy =
        Text.pack . show $ _gcSquareSize gc * y
      halfS =
        Text.pack . show $ _gcSquareSize gc `div` 2
      baseAttrs =
        "y" =: sy <>
        "width" =: halfS <>
        "height" =: s <>
        "stroke-width" =: "1"
      mkFill (Just Red) =
        "fill" =: "red" <>
        "class" =: "grid-square"
      mkFill (Just Blue) =
        "fill" =: "blue" <>
        "class" =: "grid-square"
      mkFill Nothing =
        "fill" =: "none" <>
        "stroke" =: "none"
      leftAttrs c1 =
        (mkFill <$> c1) <> pure ("x" =: sx1 <> baseAttrs)
      leftHalf c1 =
        svgDynAttr "rect" (leftAttrs c1) $
          pure ()
      rightAttrs c2 =
        (mkFill <$> c2) <> pure ("x" =: sx2 <> baseAttrs)
      rightHalf c2 =
        svgDynAttr "rect" (rightAttrs c2) $
          pure ()
    in do
      leftHalf (fmap fst <$> dPair)
      rightHalf (fmap snd <$> dPair)

mkRedBlueInput :: MonadWidget t m
               => m (Event t Colour)
mkRedBlueInput = do
  eRed <- buttonClass "btn btn-default" "Red"
  eBlue <- buttonClass "btn btn-default" "Blue"
  return $ leftmost [Red <$ eRed, Blue <$ eBlue]

