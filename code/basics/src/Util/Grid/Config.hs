{-# LANGUAGE TemplateHaskell #-}
module Util.Grid.Config (
    GridConfig(..)
  , gcRows
  , gcColumns
  , gcSquareSize
  , gcTextWidth
  , gcTextGap
  , defaultGridConfig
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
  GridConfig 5 15 40 150 30

