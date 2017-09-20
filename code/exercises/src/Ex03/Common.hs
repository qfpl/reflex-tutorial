{-# LANGUAGE OverloadedStrings #-}
module Ex03.Common (
    Money
  , Product (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Error(..)
  , Outputs(..)
  , Ex03Fn
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
  , ibSelected :: Behavior t Text
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }

data Error =
    NotEnoughMoney
  deriving (Eq, Ord, Show)

data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  }

type Ex03Fn t = Inputs t -> Outputs t

