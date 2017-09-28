{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Util.Number where

import Data.Default

import Control.Lens

import Reflex
import Reflex.Dom.Core

import qualified Util.Bootstrap as B

data NumberInputConfig t
   = NumberInputConfig { _numberInputConfig_initialValue :: Int
                       , _numberInputConfig_setValue :: Event t Int
                       }

instance Reflex t => Default (NumberInputConfig t) where
  {-# INLINABLE def #-}
  def = NumberInputConfig { _numberInputConfig_initialValue = 0
                          , _numberInputConfig_setValue = never
                          }

data NumberInput t
   = NumberInput { _numberInput_value :: Dynamic t Int
                 , _numberInput_input :: Event t Int
                 }

{-# INLINABLE numberInput #-}
numberInput ::
  ( MonadWidget t m
  ) =>
  NumberInputConfig t ->
  m (NumberInput t)
numberInput (NumberInputConfig initial eSetValue) = mdo
  dNumber <- foldDyn ($) initial . leftmost $ [
      (+ 1) <$  eAdd
    , const <$> eSetValue
    ]
  (e, _) <- el' "button" $ display dNumber
  let eAdd = domEvent Click e
      eOut = tagPromptlyDyn dNumber eAdd
  return $ NumberInput dNumber eOut

makeLenses ''NumberInputConfig
makeLenses ''NumberInput
