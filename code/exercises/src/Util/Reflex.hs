{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Util.Reflex (
    buttonDynAttr
  , buttonAttr
  , buttonDynClass
  , buttonClass
  ) where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dom.Core

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e

buttonAttr ::
  MonadWidget t m =>
  Map Text Text ->
  Text ->
  m (Event t ())
buttonAttr attrs =
  buttonDynAttr (pure attrs)

buttonDynClass ::
  MonadWidget t m =>
  Dynamic t Text ->
  Text ->
  m (Event t ())
buttonDynClass dCls =
  buttonDynAttr (("class" =:) <$> dCls)

buttonClass ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())
buttonClass cls =
  buttonDynClass (pure cls)
