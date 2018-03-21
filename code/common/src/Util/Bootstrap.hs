{-# LANGUAGE OverloadedStrings #-}
module Util.Bootstrap (
    panel
  , button
  , buttonClass
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import Reflex.Dom.Core hiding (button)

import qualified Util.Reflex as R

button ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
button =
  R.buttonClass "btn data61-green text-white p-2 mx-2"

buttonClass ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())
buttonClass cl =
  R.buttonClass ("btn data61-green text-white p-2 mx-2 " <> cl)

panel ::
  MonadWidget t m =>
  m a ->
  m a
panel =
  divClass "card my-2" .
  divClass "card-body"
