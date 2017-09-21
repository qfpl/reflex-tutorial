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
                -> Dynamic t (Maybe a)
                -> Dynamic t (Map Text Text)

  mkSquare :: MonadWidget t m
           => GridConfig
           -> Int -- y
           -> Int -- x
           -> Dynamic t (Maybe a)
           -> m ()
  mkSquare gc y x da =
    svgDynAttr "rect" (mkSquareAttrs gc y x da) $
      pure ()

instance Square () where
  mkSquareAttrs gc y x da = 
    let
      mkAttr Nothing =
        "fill" =: "none" <>
        "stroke" =: "none"
      mkAttr (Just _) =
        "fill" =: "gray" <>
        "class" =: "grid-square"
    in
      (mkAttr <$> da) <> pure (standardAttrs gc y x)
