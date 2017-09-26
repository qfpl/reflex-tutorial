{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex09.Run (
    host
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex09.Common

table ::
  MonadWidget t m =>
  m a ->
  m a
table =
  elClass "table" "table"

rowAp ::
  MonadWidget t m =>
  (a -> b -> c -> d -> e) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e
rowAp f ma mb mc md = el "tr" $ do
  a <- el "td" ma
  b <- el "td" mb
  c <- el "td" mc
  d <- el "td" md
  pure $ f a b c d

{-
table ::
  MonadWidget t m =>
  m a ->
  m a
table =
  divClass "container"

rowAp ::
  MonadWidget t m =>
  (a -> b -> c -> d -> e) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e
rowAp f ma mb mc md = divClass "row" $ do
  a <- divClass "col-md-3" ma
  b <- divClass "col-md-1" mb
  c <- divClass "col-md-1" mc
  d <- divClass "col-md-1" md
  pure $ f a b c d
-}

row ::
  MonadWidget t m =>
  m a ->
  m b ->
  m c ->
  m d ->
  m d
row =
  rowAp (\_ _ _ d -> d)

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected =
  let
    r1 = dynText $ pName . sProduct <$> dStock
    r2 = dynText $ Text.pack . show . sQuantity <$> dStock
    r3 = dynText $ moneyDisplay . pCost . sProduct <$> dStock

    attrs =
      "type" =: "radio" <>
      "name" =: "stock"
    mkAttrs s n =
      if (pName . sProduct) s == n
      then "checked" =: ""
      else mempty
    dynAttrs = mkAttrs <$> dStock <*> dSelected

    r4 = do
      (e, _) <- elDynAttr' "input" (pure attrs <> dynAttrs) $ pure ()
      let eClick = domEvent Click e
      pure $ pName . sProduct <$> current dStock <@ eClick
  in
    row r1 r2 r3 r4

host ::
  MonadWidget t m =>
  Ex09FnA t m ->
  Ex09FnB t m ->
  m ()
host mkStock fn = B.panel . table $ mdo
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

