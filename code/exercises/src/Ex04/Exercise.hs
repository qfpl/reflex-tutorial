{-# LANGUAGE CPP #-}
module Ex04.Exercise where

import qualified Data.Map    as Map
import qualified Data.Text   as Text

import           Reflex

#ifndef ghcjs_HOST_OS
import           Util.Run
#endif

import           Ex04.Common
import           Ex04.Run

ex04 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex04 (Inputs bMoney bCarrot bCelery bCucumber bSelected eBuy eRefund) =
  let
    name :: Stock -> Text.Text
    name s = pName . sProduct $ s
    -- lbStock :: [Behavior t Stock]
    lbStock =
      [bCarrot, bCelery, bCucumber]
    bName :: Reflex t
          => Behavior t Stock
          -> Behavior t (Text.Text, Stock)
    bName bs = (,) <$> (fmap name bs) <*> bs
    -- ltStock :: Behavior t (Map.Map Text.Text Stock)
    bmStock = Map.fromList <$> (sequence $ bName <$> lbStock)
    -- eStock :: Event t Stock
    eStock =
      attachWithMaybe Map.lookup bSelected (bmStock <@ eBuy)
    checkNotEnoughStock s =
      sQuantity s == 0
    checkNotEnoughMoney money p =
      money < pCost p
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id
      (checkNotEnoughMoney <$> bMoney <@> (sProduct <$> eStock))
    eItemOutOfStock =
      ItemOutOfStock <$ ffilter id
      (checkNotEnoughStock <$> eStock)
    eSale =
      difference eStock eError
    eVend =
      pName . sProduct <$> eSale
    eSpend =
      pCost . sProduct <$> eSale
    eChange =
      bMoney <@ eRefund
    eError =
      leftmost [eItemOutOfStock, eNotEnoughMoney]
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex04
#endif
