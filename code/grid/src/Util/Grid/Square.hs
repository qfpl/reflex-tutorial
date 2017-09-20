{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Util.Grid.Square (
    Square(..)
  , standardAttrs
  , drawGridEventSquare
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
  mkSquare :: MonadWidget t m
           => GridConfig
           -> Int -- y
           -> Int -- x
           -> a
           -> m ()

instance Square () where
  mkSquare gc y x _ =
    let
      attrs =
        "class" =: "grid-square" <>
        "fill" =: "gray" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

instance Square a => Square (Maybe a) where
  mkSquare gc y x Nothing =
    let
      attrs =
        "fill" =: "none" <>
        "stroke" =: "none" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()
  mkSquare gc y x (Just a) =
    mkSquare gc y x a

drawGridEventSquare ::
  ( MonadWidget t m
  , Square a
  ) =>
  GridConfig ->
  Int ->
  Int ->
  Dynamic t (Maybe a) ->
  m (Event t ())
drawGridEventSquare gc y x dValue = do
  dyn $ mkSquare gc y x <$> dValue
