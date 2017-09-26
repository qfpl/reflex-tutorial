{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex07.Solution (
    attachEx07
  ) where

import Language.Javascript.JSaddle (JSM)

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex07.Common
import Ex07.Run

ex07 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
-- We use `mdo` here, because we're starting to use logic that
-- is more easily expressed if we can have cycles in the FRP network
ex07 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
  let
    dStocks =
      [dCarrot, dCelery, dCucumber]
    stockSingleton s =
      Map.singleton (pName . sProduct $ s) s
    dmStock =
      foldMap (fmap stockSingleton) dStocks
    emStock =
      Map.lookup <$> current dSelected <*> current dmStock <@ eBuy
    eStock =
      fmapMaybe id emStock

    checkItemOutOfStock s =
      sQuantity s == 0
    eItemOutOfStock =
      ItemOutOfStock <$ ffilter checkItemOutOfStock eStock

    checkNotEnoughMoney money s =
      money < (pCost . sProduct $ s)
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> current dMoney <@> eStock)

    eError =
      leftmost [
        eItemOutOfStock
      , eNotEnoughMoney
      ]

    eSale =
      sProduct <$> difference eStock eError

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale

    eChange =
      current dMoney <@ eRefund

  -- We have separated out the tracking of the money into a
  -- function called `trackMoney`
  dMoney  <- trackMoney eAdd eSpend eRefund

  dChange <- changeDisplay eSpend eChange eError
  dVend   <- vendDisplay eVend eSpend eError

  pure $ Outputs eVend eSpend eChange eError dMoney dChange dVend

trackMoney ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t () ->
  Event t Money ->
  Event t () ->
  m (Dynamic t Money)
trackMoney eAdd eSpend eRefund = mdo
  -- There are many, many ways that we can do this.

  -- If what you have works, I'd call it a win - you'll
  -- almost definitely develop a personal style as you
  -- solve problems with FRP, and that's a good thing.
  let
    isOverspend money price =
      money < price
    eOverspend =
      isOverspend <$> current dMoney <@> eSpend
    eSpendOK =
      difference eSpend (ffilter id eOverspend)

  -- If we didn't want to worry about stopping this from going negative
  -- then we could use `eSpend` in place of `eSpendOK`.
  dMoney <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)    <$  eAdd
    , flip (-) <$> eSpendOK
    , const 0  <$  eRefund
    ]

  pure dMoney

changeDisplay ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t Money ->
  Event t Money ->
  Event t Error ->
  m (Dynamic t Money)
changeDisplay eSpend eChange eError =
  holdDyn 0 .  leftmost $ [
      eChange
    , 0 <$ eSpend
    , 0 <$ eError
    ]

vendDisplay ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t Text ->
  Event t Money ->
  Event t Error ->
  m (Dynamic t Text)
vendDisplay eVend eSpend eError =
  holdDyn "" .  leftmost $ [
     eVend
   , ""        <$  eSpend
   , errorText <$> eError
   ]

attachEx07 ::
  JSM ()
attachEx07 =
  attachId_ "ex07" $
    host ex07

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex07
#endif
