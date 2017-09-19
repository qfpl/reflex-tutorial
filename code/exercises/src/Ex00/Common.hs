module Ex00.Common (
    Ex00Fn
  ) where

import Data.Text (Text)

import Reflex

type Ex00Fn t =
  Event t () ->
  Event t () ->
  (Event t Text, Event t Text)

