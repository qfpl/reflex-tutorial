{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex01.Solution (
    attachEx01
  ) where

import Language.Javascript.JSaddle (JSM)

import Reflex

import Util.Attach
#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex01.Common
import Ex01.Run

ex01 ::
  Reflex t =>
  Int ->
  Inputs t ->
  Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    {-
    We have access to values of `Product`: `carrot`, `celery` and `cucumber`.

    We can use the `Functor` instance for `Event`s to convert each of the purchase `Event`s of type `()`
    into `Event`s of type `Product`.

    For example: `carrot <$ eCarrot` will have type `Event t Product` and will fire when anyone presses
    the "Buy carrot" button on our virtual vending machine.

    The last thing we need to do that will simplify matters for us is to combine these `Event`s.

    We will bake in the assumption that they are coming from buttons which can't be pressed simultaneously
    and use `leftmost` to combine them.
    -}
    eProduct =
      leftmost [
          carrot   <$ eCarrot
        , celery   <$ eCelery
        , cucumber <$ eCucumber
        ]

    -- We write predicate to check whether there isn't enough in the machine for a given product:
    checkNotEnoughMoney p =
      money < pCost p
    -- and we use that to along with `ffilter` to create our error `Event`, and we tag it with `()`
    -- so that the types work out:
    eNotEnoughMoney =
      () <$ ffilter checkNotEnoughMoney eProduct

    -- We can use `difference` to create an `Event` that will fire with the selected `Product`
    -- as long as there is enough money in the machine to pay for it:
    eSale =
      difference eProduct eNotEnoughMoney

    -- We can pull apart the `Product` to get hold of name:
    eVend =
      pName <$> eSale
    -- and price:
    eSpend =
      pCost <$> eSale
    -- of the purchased `Product`

    -- Finally, we pass the money in the machine through whenever the "Refund" button is pressed:
    eChange =
      money <$ eRefund
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

attachEx01 ::
  JSM ()
attachEx01 =
  attachId_ "ex01" $
    host ex01

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif
