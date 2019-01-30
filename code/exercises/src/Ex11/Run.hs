{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Ex11.Run (
    radioCheckbox
  , host
  ) where

import Control.Lens

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex11.Common

radioCheckbox ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioCheckbox dValue dSelected =
  let
    dMatch = (==) <$> dValue <*> dSelected
  in do
    ePostBuild <- getPostBuild
    let eChanges = leftmost [
            updated dMatch
          , current dMatch <@ ePostBuild
          ]
    cb <- checkbox False $
      def & checkboxConfig_setValue .~ eChanges
    pure $ current dValue <@ ffilter id (cb ^. checkbox_change)

host ::
  MonadWidget t m =>
  Ex11FnGrid m ->
  Ex11FnStockWidget t m ->
  Ex11FnMkStock t m ->
  Ex11FnMain t m ->
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

