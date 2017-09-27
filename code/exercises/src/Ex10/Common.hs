{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Ex10.Common (
    Money
  , moneyDisplay
  , Product (..)
  , Stock (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , Error(..)
  , errorText
  , Ex10FnGrid
  , Ex10FnRow
  , Ex10FnMkStock
  , Ex10FnMain
  ) where

import Data.Monoid ((<>))

import Data.Text
import qualified Data.Text as Text

import Reflex

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

data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
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

type Ex10FnGrid m =
  forall a.
  m a ->
  m a

type Ex10FnRow m =
  forall a b c d.
  m a ->
  m b ->
  m c ->
  m d ->
  m d

type Ex10FnMkStock t m =
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)

type Ex10FnMain t m =
  Inputs t ->
  m (Event t Text)

