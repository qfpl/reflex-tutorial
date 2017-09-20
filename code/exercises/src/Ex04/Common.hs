{-# LANGUAGE OverloadedStrings #-}
module Ex04.Common (
    Money
  , Product (..)
  , Stock (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Error(..)
  , Outputs(..)
  , Ex04Fn
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
    ibMoney    :: Behavior t Money
  , ibCarrot   :: Behavior t Stock
  , ibCelery   :: Behavior t Stock
  , ibCucumber :: Behavior t Stock
  , ibSelected :: Behavior t Text
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }

data Error =
    NotEnoughMoney
  | ItemOutOfStock
  deriving (Eq, Ord, Show)

data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  }

type Ex04Fn t = Inputs t -> Outputs t

