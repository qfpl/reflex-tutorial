{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
module Ex12.Run (
    host
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex12.Common

host ::
  MonadWidget t m =>
  Ex12FnGrid m ->
  Ex12FnStockWidget t m ->
  Ex12FnMkStock t m ->
  Ex12FnMain t m ->
  m ()
host grid stockWidget mkStock fn = B.panel . grid $ mdo
  dCarrot   <- mkStock 5 carrot   eVend
  dCelery   <- mkStock 5 celery   eVend
  dCucumber <- mkStock 5 cucumber eVend

  input <- mdo
      eCarrot <-
        stockWidget dCarrot dSelected
      eCelery <-
        stockWidget dCelery dSelected
      eCucumber <-
        stockWidget dCucumber dSelected
      dSelected <-
        holdDyn (pName carrot) .
        leftmost $ [
            eCarrot
          , eCelery
          , eCucumber
          ]
      pure $
        Inputs
          dCarrot
          dCelery
          dCucumber
          dSelected

  eVend <- fn input

  pure ()

