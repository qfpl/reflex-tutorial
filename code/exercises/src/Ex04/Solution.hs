{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex04.Solution (
    attachEx04
  ) where

import Language.Javascript.JSaddle (JSM)

import Data.Monoid ((<>))

import Data.Map (Map)
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
    stockSingleton s =
      Map.singleton (pName . sProduct $ s) s
    bmStock =
      foldMap (fmap stockSingleton) [bCarrot, bCelery, bCucumber]
    eSelected =
      fmapMaybe id (Map.lookup <$> bSelected <*> bmStock <@ eBuy)

    checkItemOutOfStock s =
      sQuantity s == 0
    eItemOutOfStock =
      ItemOutOfStock <$ ffilter checkItemOutOfStock eSelected

    checkNotEnoughMoney money s =
      money < (pCost . sProduct $ s)
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eSelected)

    eError =
      leftmost [
        eItemOutOfStock
      , eNotEnoughMoney
      ]

    eSale =
      sProduct <$> difference eSelected eError

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
