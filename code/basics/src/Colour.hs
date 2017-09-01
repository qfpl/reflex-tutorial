{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
  mkSquare gc y x c =
    let
      mkFill Red =
        "red"
      mkFill Blue =
        "blue"
      attrs =
        "class" =: "grid-square" <>
        "fill" =: mkFill c <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

instance Square (Colour, Colour) where
  mkSquare gc y x (c1, c2) =
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
        "class" =: "grid-square" <>
        "y" =: sy <>
        "width" =: halfS <>
        "height" =: s <>
        "stroke-width" =: "1"
      mkFill Red =
        "red"
      mkFill Blue =
        "blue"
      leftAttrs =
        "fill" =: mkFill c1 <>
        "x" =: sx1 <>
        baseAttrs
      leftHalf =
        svgAttr "rect" leftAttrs $
          pure ()
      rightAttrs =
        "fill" =: mkFill c2 <>
        "x" =: sx2 <>
        baseAttrs
      rightHalf =
        svgAttr "rect" rightAttrs $
          pure ()
    in do
      leftHalf
      rightHalf

mkRedBlueInput :: MonadWidget t m
               => m (Event t Colour)
mkRedBlueInput = do
  eRed <- buttonClass "btn btn-default" "Red"
  eBlue <- buttonClass "btn btn-default" "Blue"
  return $ leftmost [Red <$ eRed, Blue <$ eBlue]

