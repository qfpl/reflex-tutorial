{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
module Ex13.Run (
    host
  ) where

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex13.Common

host ::
  MonadWidget t m =>
  Ex13FnGrid m ->
  Ex13FnStockWidget t m ->
  Ex13FnMkStock t m ->
  Ex13FnMain t m ->
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

