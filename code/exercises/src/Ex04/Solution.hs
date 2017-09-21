{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex04.Solution (
    attachEx04
  ) where

import Language.Javascript.JSaddle (JSM)

import qualified Data.Map as Map

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex04.Common
import Ex04.Run

ex04 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex04 (Inputs bMoney bCarrot bCelery bCucumber bSelected eBuy eRefund) =
  let
    -- This time we have a list of `Behavior t Stock`:
    bStocks =
      [bCarrot, bCelery, bCucumber]
    -- which means we have a slightly different helper function:
    stockSingleton s =
      Map.singleton (pName . sProduct $ s) s
   -- that gets used in a slightly different way:
    bmStock =
      foldMap (fmap stockSingleton) bStocks
    emStock =
      Map.lookup <$> bSelected <*> bmStock <@ eBuy
    eStock =
      fmapMaybe id emStock

    -- We add a check to see if there is stock remaining:
    checkItemOutOfStock s =
      sQuantity s == 0
    eItemOutOfStock =
      ItemOutOfStock <$ ffilter checkItemOutOfStock eStock

    -- and adapt the money check to work with `Stock`:
    checkNotEnoughMoney money s =
      money < (pCost . sProduct $ s)
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eStock)

    -- Now we have multiple errors, so we combine them.
    -- They could both occur at once, so we give priority to "Item Out Of Stock",
    -- in the hope that we can still make the sale:
    eError =
      leftmost [
        eItemOutOfStock
      , eNotEnoughMoney
      ]

    -- We pass the sale through if no error occurred, stripping the quantity information off as we go:
    eSale =
      sProduct <$> difference eStock eError

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale

    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

attachEx04 ::
  JSM ()
attachEx04 =
  attachId_ "ex04" $
    host ex04

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex04
#endif
