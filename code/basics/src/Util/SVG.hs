{-# LANGUAGE OverloadedStrings #-}
module Util.SVG (
    svgDynAttr
  , svgAttr
  , svg
  ) where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dom.Core

svgDynAttr :: MonadWidget t m
           => Text
           -> Dynamic t (Map Text Text)
           -> m a
           -> m a
svgDynAttr n a r = do
  (_, x) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") n a r
  return x

svgAttr :: MonadWidget t m
        => Text
        -> Map Text Text
        -> m a
        -> m a
svgAttr n a r =
  svgDynAttr n (pure a) r

svg :: MonadWidget t m
    => Text
    -> m a
    -> m a
svg n r =
  svgAttr n mempty r

