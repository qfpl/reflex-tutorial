{-# LANGUAGE OverloadedStrings #-}
module Ex07.Common (
    Money
  , Product (..)
  , Stock (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Outputs(..)
  , Error(..)
  , errorText
  , Ex07Fn
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

data Stock =
  Stock {
    sProduct  :: Product
  , sQuantity :: Int
  } deriving (Eq, Ord, Show)

data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  , ieAdd      :: Event t ()
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }

data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  , odMoney  :: Dynamic t Money
  , odChange :: Dynamic t Money
  , odVend   :: Dynamic t Text
  }

data Error =
    NotEnoughMoney
  | ItemOutOfStock
  deriving (Eq, Ord, Show)

errorText ::
  Error ->
  Text
errorText NotEnoughMoney =
  "Insufficient funds"
errorText ItemOutOfStock =
  "Item out of stock"

type Ex07Fn t m = Inputs t -> m (Outputs t)

