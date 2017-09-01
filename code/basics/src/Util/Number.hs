{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Util.Number where

import Data.Default

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens

import GHCJS.DOM.HTMLInputElement
import Reflex
import Reflex.Dom.Core

data NumberInputConfig t
   = NumberInputConfig { _numberInputConfig_initialValue :: Float
                       , _numberInputConfig_setValue :: Event t Float
                       , _numberInputConfig_attributes :: Dynamic t (Map Text Text)
                       }

instance Reflex t => Default (NumberInputConfig t) where
  {-# INLINABLE def #-}
  def = NumberInputConfig { _numberInputConfig_initialValue = 0
                          , _numberInputConfig_setValue = never
                          , _numberInputConfig_attributes = constDyn mempty
                          }

data NumberInput t
   = NumberInput { _numberInput_value :: Dynamic t Float
                 , _numberInput_input :: Event t Float
                 , _numberInput_hasFocus :: Dynamic t Bool
                 , _numberInput_element :: HTMLInputElement
                 }

-- | Create an input whose value is a float.
--   https://www.w3.org/wiki/HTML/Elements/input/number
{-# INLINABLE numberInput #-}
numberInput :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => NumberInputConfig t -> m (NumberInput t)
numberInput (NumberInputConfig initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "type" "number") dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ (Text.pack . show $ initial)
    & inputElementConfig_setValue .~ (Text.pack . show <$> eSetValue)
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ NumberInput
    { _numberInput_value = read . Text.unpack <$> _inputElement_value i
    , _numberInput_input = read . Text.unpack <$> _inputElement_input i
    , _numberInput_hasFocus = _inputElement_hasFocus i
    , _numberInput_element = _inputElement_raw i
    }

makeLenses ''NumberInputConfig
makeLenses ''NumberInput
