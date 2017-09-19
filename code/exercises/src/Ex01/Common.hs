{-# LANGUAGE OverloadedStrings #-}
module Ex01.Common (
    Money
  , Product (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Outputs(..)
  , Ex01Fn
  ) where

import Data.Text

import Reflex

type Money = Int

data Product =
  Product {
    pName :: Text
  , pCost :: Money
  }

carrot ::
  Product
carrot =
  Product "Carrot" 1

celery ::
  Product
celery =
  Product "Celery" 2

cucumber ::
  Product
cucumber =
  Product "Cucumber" 3

data Inputs t =
  Inputs {
    ieCarrot   :: Event t ()
  , ieCelery   :: Event t ()
  , ieCucumber :: Event t ()
  , ieRefund   :: Event t ()
  }

data Outputs t =
  Outputs {
    oeVend           :: Event t Text
  , oeSpend          :: Event t Money
  , oeChange         :: Event t Money
  , oeNotEnoughMoney :: Event t ()
  }

type Ex01Fn t = Int -> Inputs t -> Outputs t

