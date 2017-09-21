{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex03.Solution (
    attachEx03
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

import Ex03.Common
import Ex03.Run

ex03 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    productSingleton p =
      Map.singleton (pName p) (pure p)
    mbProduct =
      foldMap productSingleton [carrot, celery, cucumber]
    bmProduct =
      sequence mbProduct
    eSelected =
      fmapMaybe id (Map.lookup <$> bSelected <*> bmProduct <@ eBuy)

    checkNotEnoughMoney money p =
      money < pCost p
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eSelected)

    eError =
      eNotEnoughMoney

    eSale =
      difference eSelected eError

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
