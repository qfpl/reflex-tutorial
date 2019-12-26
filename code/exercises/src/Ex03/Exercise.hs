{-# LANGUAGE CPP #-}
module Ex03.Exercise where

import qualified Data.Map    as Map

import           Reflex

#ifndef ghcjs_HOST_OS
import           Util.Run
#endif

import           Ex03.Common
import           Ex03.Run

ex03 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    products =
      Map.fromList $ toTuple <$>
      [ carrot, celery, cucumber ]
      where
        toTuple p@(Product n _) = (n, p)
    getProduct = (flip Map.lookup) products
    checkNotEnoughMoney money p =
      money < pCost p
    eProduct =
      fmapMaybe getProduct $
      bSelected <@ eBuy
    eSale =
      difference eProduct eError
    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
    eError =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eProduct)
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex03
#endif
