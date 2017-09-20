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
    bStock =
      foldMap (fmap stockSingleton) [bCarrot, bCelery, bCucumber]
    eSelected =
      fmapMaybe id $
      Map.lookup <$> bSelected <*> bStock <@ eBuy

    filterQuantity p =
      fmap sProduct .
      ffilter (p . sQuantity)
    eItemOutOfStock =
      filterQuantity (== 0) eSelected
    eItemInStock =
      filterQuantity (> 0) eSelected

    filterCost p e =
      fmap snd .
      ffilter fst $
      (\m pr -> (p m pr, pr)) <$> bMoney <@> e

    eNotEnoughMoney =
      filterCost (\m p -> m < pCost p) eItemInStock
    eSale =
      filterCost (\m p -> m >= pCost p) eItemInStock

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
    eError =
      leftmost [
        ItemOutOfStock <$ eItemOutOfStock
      , NotEnoughMoney <$ eNotEnoughMoney
      ]
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
