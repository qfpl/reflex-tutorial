{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex03.Solution (
    attachEx03
  ) where

import Language.Javascript.JSaddle (JSM)

import qualified Data.Map as Map

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex03.Common
import Ex03.Run

ex03 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    -- We put our products in a list:
    products =
      [carrot, celery, cucumber]
    -- We write a helper function to get from a `Product` to a `Map Text (Behavior t Product)`
    productSingleton p =
      Map.singleton (pName p) (pure p)
    -- We use this helper to turn our products into a `Map` and then combine the `Map`s, using `foldMap`:
    mbProduct =
      foldMap productSingleton products
    -- We have a `Map` of `Behavior`s that we want to turn into a `Behavior` of `Map`s,
    -- so we use `sequence`:
    bmProduct =
      sequence mbProduct
    -- We use `(<@)` here to run `Map.lookup` with the values of `bSelected` and `bmProduct` at
    -- the times that `eBuy` fires:
    emProduct =
      Map.lookup <$> bSelected <*> bmProduct <@ eBuy
    -- Finally, we use `fmapMaybe id` to filter out the `Nothing` values and removing the `Just` constructor:
    eProduct =
      fmapMaybe id emProduct

    checkNotEnoughMoney money p =
      money < pCost p
    eError =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eProduct)

    eSale =
      difference eProduct eError

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

attachEx03 ::
  JSM ()
attachEx03 =
  attachId_ "ex03" $
    host ex03

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex03
#endif
