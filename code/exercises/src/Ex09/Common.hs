{-# LANGUAGE OverloadedStrings #-}
module Ex09.Common (
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
  , table
  , row
  , row_
  , row'
  , Ex09FnA
  , Ex09FnB
  ) where

import Data.Monoid ((<>))

import Data.Text
import qualified Data.Text as Text

import Reflex
import Reflex.Dom.Core

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

table ::
  MonadWidget t m =>
  m a ->
  m a
table =
  elClass "table" "table"

row ::
  MonadWidget t m =>
  (a -> b -> c -> d -> e) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e
row f ma mb mc md = el "tr" $ do
  a <- el "td" ma
  b <- el "td" mb
  c <- el "td" mc
  d <- el "td" md
  pure $ f a b c d

{-
table ::
  MonadWidget t m =>
  m a ->
  m a
table =
  divClass "container"

row ::
  MonadWidget t m =>
  (a -> b -> c -> d -> e) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e
row f ma mb mc md = divClass "row" $ do
  a <- divClass "col-md-3" ma
  b <- divClass "col-md-1" mb
  c <- divClass "col-md-1" mc
  d <- divClass "col-md-1" md
  pure $ f a b c d
-}

row_ ::
  MonadWidget t m =>
  m a ->
  m b ->
  m c ->
  m d ->
  m ()
row_ =
  row (\_ _ _ _ -> ())

row' ::
  MonadWidget t m =>
  m a ->
  m b ->
  m c ->
  m d ->
  m d
row' =
  row (\_ _ _ d -> d)

type Ex09FnA t m =
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)

type Ex09FnB t m =
  Inputs t ->
  m (Event t Text)

