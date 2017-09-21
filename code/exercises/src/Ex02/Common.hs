{-# LANGUAGE OverloadedStrings #-}
module Ex02.Common (
    Money
  , Product (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Outputs(..)
  , Error(..)
  , errorText
  , Ex02Fn
  ) where

import Data.Text

import Reflex

type Money = Int

data Product =
  Product {
    pName :: Text
  , pCost :: Money
  } deriving (Eq, Ord, Show)

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
    ibMoney    :: Behavior t Money
  , ieCarrot   :: Event t ()
  , ieCelery   :: Event t ()
  , ieCucumber :: Event t ()
  , ieRefund   :: Event t ()
  }

data Outputs t =
  Outputs {
    oeVend           :: Event t Text
  , oeSpend          :: Event t Money
  , oeChange         :: Event t Money
  , oeNotEnoughMoney :: Event t Error
  }

data Error =
    NotEnoughMoney
  deriving (Eq, Ord, Show)

errorText ::
  Error ->
  Text
errorText NotEnoughMoney =
  "Insufficient funds"

type Ex02Fn t = Inputs t -> Outputs t

