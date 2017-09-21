{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex02.Solution (
    attachEx02
  ) where

import Language.Javascript.JSaddle (JSM)

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex02.Common
import Ex02.Run

ex02 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
  let
    eProduct =
      leftmost [
          carrot   <$ eCarrot
        , celery   <$ eCelery
        , cucumber <$ eCucumber
        ]

    checkNotEnoughMoney money p =
      money < pCost p
    {-
    We use `(<@>)` here to run `checkNotEnoughMoney` with the value of `bMoney` and `eProduct` at
    the times that `eProduct` fires.

    After that we filter the `Event` as before, and then we add our `Error` value onto the `Event`.
    -}
    eError =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eProduct)

    eSale =
      difference eProduct eError

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale

    -- We can use `(<@)` or `tag` here to sample `bMoney` when the "Refund" button is pressed
    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

attachEx02 ::
  JSM ()
attachEx02 =
  attachId_ "ex02" $
    host ex02

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex02
#endif
