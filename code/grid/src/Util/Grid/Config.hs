{-# LANGUAGE TemplateHaskell #-}
module Util.Grid.Config (
    GridConfig(..)
  , gcRows
  , gcColumns
  , gcSquareSize
  , gcTextWidth
  , gcTextGap
  , defaultGridConfig
  , gridWidth
  , gridHeight
  , gridXOffset
  , gridYOffset
  , wholeWidth
  , wholeHeight
  ) where

import Control.Lens.TH

data GridConfig =
  GridConfig {
    _gcRows :: Int
  , _gcColumns :: Int
  , _gcSquareSize :: Int
  , _gcTextWidth :: Int
  , _gcTextGap :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''GridConfig

defaultGridConfig :: GridConfig
defaultGridConfig =
  GridConfig 5 12 40 150 30

gridWidth ::
  GridConfig ->
  Int
gridWidth gc =
  _gcSquareSize gc * _gcColumns gc

gridHeight ::
  GridConfig ->
  Int
gridHeight gc =
  _gcSquareSize gc * _gcRows gc

gridXOffset ::
  GridConfig ->
  Int
gridXOffset gc =
  _gcTextWidth gc

gridYOffset ::
  GridConfig ->
  Int
gridYOffset _ =
  0

wholeWidth ::
  GridConfig ->
  Int
wholeWidth gc =
  _gcTextWidth gc + gridWidth gc

wholeHeight ::
  GridConfig ->
  Int
wholeHeight gc =
  gridHeight gc
