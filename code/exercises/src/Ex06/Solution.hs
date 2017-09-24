{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex06.Solution (
    attachEx06
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

import Ex06.Common
import Ex06.Run

ex06 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex06 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
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
  -- We start to use a `do` block to build up these `Dynamic`s
  in do

    -- We can get away with `holdDyn` for both of these, and
    -- then it's just a matter of remember which `Event`s we
    -- want to involve.

    dChange <- holdDyn 0 .  leftmost $ [
        eChange
      , 0 <$ eSpend
      , 0 <$ eError
      ]

    dVend <-holdDyn "" .  leftmost $ [
        eVend
      , ""        <$  eSpend
      , errorText <$> eError
      ]

    -- If you're doing the optional extra and breaking these
    -- out into their own functions, they appear below as
    -- `changeDisplay` and `vendDisplay`.

    pure $ Outputs eVend eSpend eChange eError dChange dVend

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

attachEx06 ::
  JSM ()
attachEx06 =
  attachId_ "ex06" $
    host ex06

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif
