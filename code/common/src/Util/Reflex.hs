{-# LANGUAGE OverloadedStrings #-}
module Util.Reflex (
    buttonClass
  ) where

import Reflex.Dom.Core

import Data.Text (Text)

buttonClass :: MonadWidget t m
            => Text
            -> Text
            -> m (Event t ())
buttonClass cl label = do
  (e, _) <- elAttr' "button" ("class" =: cl) $
             text label
  return $ domEvent Click e

