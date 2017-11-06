{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Posts.Collection.Common (
    addItemWidget
  , clearComplete
  , markAllComplete
  , TodoItem(..)
  , tiComplete
  , tiText
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

addItemWidget ::
  MonadWidget t m =>
  m (Event t Text)
addItemWidget = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eClear)

  let
    dValue = ti ^. textInput_value

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress
    eEscape   = ffilter (isKey Escape) eKeypress

    eAtEnter  = Text.strip <$> current dValue <@ eEnter

    eDone = ffilter (not . Text.null) eAtEnter
    eClear = leftmost [void eDone, void eEscape]

  pure eDone

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAnyComplete =
  let
    mkClass False = "hide"
    mkClass True  = ""
    dClass = mkClass <$> dAnyComplete
  in
    elDynClass "div" dClass $
      button "Clear complete"

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = el "div" $ do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText     :: Text
  }

makeLenses ''TodoItem
