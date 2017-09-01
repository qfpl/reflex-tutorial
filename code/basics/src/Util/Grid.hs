{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Util.Grid (
    GridConfig(..)
  , gcRows
  , gcColumns
  , gcSquareSize
  , gcTextWidth
  , gcTextGap
  , defaultGridConfig
  , setupGrid
  , drawGrid
  , Row(..)
  , Square(..)
  , standardAttrs
  ) where

import Data.Foldable (traverse_)
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Util.SVG

import Util.Grid.Config
import Util.Grid.Square

data Row t where
  Row :: Square a
      => Text
      -> Int
      -> Dynamic t [Maybe a]
      -> Row t

rowLabel :: Row t -> Text
rowLabel (Row t _ _) = t

row :: Row t -> Int
row (Row _ i _) = i

drawGridEventLabel ::
  MonadWidget t m =>
  GridConfig ->
  Row t ->
  m ()
drawGridEventLabel gc r =
  let
    x = Text.pack . show $ _gcTextWidth gc - _gcTextGap gc
    y = Text.pack . show $ _gcSquareSize gc * (1 + row r)
    fontSize = Text.pack . show . (`div` 4) . (* 3) . _gcSquareSize $ gc
    labelAttrs =
      "class" =: "grid-label" <>
      "x" =: x <>
      "y" =: y <>
      "font-size" =: fontSize <>
      "text-anchor" =: "end"
  in
    svgAttr "text" labelAttrs . text . rowLabel $ r

drawGridEventSquares ::
  ( MonadWidget t m
  , Square a
  ) =>
  GridConfig ->
  Int ->
  Dynamic t [Maybe a] ->
  m ()
drawGridEventSquares gc row dValues = do
  let
    tidy = Map.fromList . zip [0..] . reverse . take (_gcColumns gc)
    dTidy = tidy <$> dValues
  _ <- listViewWithKey dTidy (drawGridEventSquare gc row)
  pure ()

width :: GridConfig -> Text
width gc = Text.pack . show $
  1 + _gcTextWidth gc + _gcColumns gc * _gcSquareSize gc

height :: GridConfig -> Text
height gc = Text.pack . show $
  1 + _gcRows gc * _gcSquareSize gc

outerDivAttrs :: GridConfig -> Map Text Text
outerDivAttrs gc =
  let
    style = Text.concat ["width:", width gc, "px;height:", height gc, "px"]
  in
    "style" =: style

baseSvgAttrs :: Map Text Text
baseSvgAttrs =
  "xmlns" =: "http://www.w3.org/2000/svg" <>
  "baseProfile" =: "full" <>
  "version" =: "1.1"

svgAttrs :: GridConfig -> Map Text Text
svgAttrs gc =
  let
    viewBox = Text.concat ["0 0 ", width gc, " ", height gc]
  in
    "width" =: "100%" <>
    "height" =: "100%" <>
    "viewBox" =: viewBox <>
    baseSvgAttrs

gridPatternAttrs :: GridConfig -> Map Text Text
gridPatternAttrs gc =
  let
    s = Text.pack . show . _gcSquareSize $ gc
    d = Text.concat [s, "px"]
  in
    "id" =: "grid" <>
    "patternUnits" =: "userSpaceOnUse" <>
    "width" =: d <>
    "height" =: d

gridPathAttrs :: GridConfig -> Map Text Text
gridPathAttrs gc =
  let
    s = Text.pack . show . _gcSquareSize $ gc
    d = Text.concat ["M ", s, " 0 L 0 0 0 ", s]
  in
    "class" =: "grid-line" <>
    "fill" =: "none" <>
    "stroke-width" =: "1" <>
    "d" =: d

gridTransformAttrs :: GridConfig -> Map Text Text
gridTransformAttrs gc =
  let
    w = Text.pack . show . _gcTextWidth $ gc
  in
  "transform" =: Text.concat ["translate(", w, ",0)"]

gridRectAttrs :: Map Text Text
gridRectAttrs =
  "width" =: "100%" <>
  "height" =: "100%" <>
  "fill" =: "url(#grid)"

setupGrid ::
  MonadWidget t m =>
  GridConfig ->
  m ()
setupGrid gc =
  el "div" $
    svgAttr "svg" ("height" =: "0" <> baseSvgAttrs) $ do
      svg "defs" $
        svgAttr "pattern" (gridPatternAttrs gc) $ do
          svgAttr "path" (gridPathAttrs gc) $
            pure ()

drawGrid ::
  MonadWidget t m =>
  GridConfig ->
  [Row t] ->
  m ()
drawGrid gc rows =
  let
    drawGridEventSquares' r =
      case r of
        Row _ r d -> drawGridEventSquares gc r d
    labels =
      traverse_ (drawGridEventLabel gc) rows
    squares =
      traverse_ drawGridEventSquares' rows
  in
    elAttr "div" (outerDivAttrs gc) $
      svgAttr "svg" (svgAttrs gc) $ do

        labels

        svgAttr "g" (gridTransformAttrs gc) $ do
          svgAttr "rect" gridRectAttrs $
            pure ()
          squares

