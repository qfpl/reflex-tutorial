{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Util.Grid.Square (
    Square(..)
  , standardAttrs
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)

import Reflex.Dom.Core

import Util.SVG
import Util.Grid.Config

standardAttrs :: GridConfig
              -> Int
              -> Int
              -> Map Text Text
standardAttrs gc y x =
  let
    s =
      Text.pack . show $ _gcSquareSize gc
    sx =
      Text.pack . show $ _gcSquareSize gc * x
    sy =
      Text.pack . show $ _gcSquareSize gc * y
  in
    "x" =: sx <>
    "y" =: sy <>
    "width" =: s <>
    "height" =: s <>
    "stroke-width" =: "1"

class Square a where
  mkSquareAttrs :: Reflex t
                => GridConfig
                -> Int -- y
                -> Int -- x
                -> Dynamic t a
                -> Dynamic t (Map Text Text)

  mkSquare :: MonadWidget t m
           => GridConfig
           -> Int -- y
           -> Int -- x
           -> Dynamic t a
           -> m ()
  mkSquare gc y x da =
    svgDynAttr "rect" (mkSquareAttrs gc y x da) $
      pure ()

instance Square () where
  mkSquareAttrs gc y x _ = pure $
    "class" =: "grid-square" <>
    "fill" =: "gray" <>
    standardAttrs gc y x

instance Square a => Square (Maybe a) where
  mkSquareAttrs gc y x da =
    let
      f Nothing = pure $
        "fill" =: "none" <>
        "stroke" =: "none" <>
        standardAttrs gc y x
      f (Just a) =
        mkSquareAttrs gc y x (pure a)
    in
      da >>= f
