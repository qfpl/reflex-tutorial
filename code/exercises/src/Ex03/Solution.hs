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
    bProduct =
      sequence . foldMap productSingleton $ [carrot, celery, cucumber]
    eSelected =
      fmapMaybe id $
      Map.lookup <$> bSelected <*> bProduct <@ eBuy

    filterCost p e =
      fmap snd .
      ffilter fst $
      (\m pr -> (p m pr, pr)) <$> bMoney <@> e

    eNotEnoughMoney =
      filterCost (\m p -> m < pCost p) eSelected
    eSale =
      filterCost (\m p -> m >= pCost p) eSelected

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
    eError =
      NotEnoughMoney <$ eNotEnoughMoney
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
