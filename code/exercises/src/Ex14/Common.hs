{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Ex14.Common (
    Money
  , moneyDisplay
  , Product (..)
  , Stock (..)
  , carrot
  , celery
  , cucumber
  , Error(..)
  , errorText
  ) where

import Data.Monoid ((<>))

import Data.Text
import qualified Data.Text as Text

type Money = Int

moneyDisplay ::
  Money ->
  Text
moneyDisplay =
  ("$" <>) . Text.pack . show

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

