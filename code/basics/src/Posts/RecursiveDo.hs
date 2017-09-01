{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.RecursiveDo (
    recursiveDoPostExamples
  ) where

import Data.Maybe (fromMaybe)

import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

recursiveDoPostExamples ::
  MonadJSM m =>
  m ()
recursiveDoPostExamples = do
  mdLimit <- attachId "basics-recursiveDo-1" $
    counterExample1 5
  mdStep <- attachId "basics-recursiveDo-2" $
    counterExample1 1
  attachId_ "basics-recursiveDo-3" $ do
    let
      dLimit = fromMaybe (pure 5) mdLimit
      dStep = fromMaybe (pure 1) mdStep
    counterExample2 dLimit dStep
  attachId_ "basics-recursiveDo-4"
    bigExample

mkCounter ::
  MonadWidget t m =>
  (Event t () -> Event t () -> m (Dynamic t Int)) ->
  m (Dynamic t Int)
mkCounter network = mdo
  el "div" $
    display dCount

  (eAdd, eClear) <- el "span" $
    (,) <$> button "Add" <*> button "Clear"

  dCount <- network eAdd eClear

  pure dCount

counter1 ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Event t () ->
  Event t () ->
  m (Dynamic t Int)
counter1 initial eAdd eClear =
  foldDyn ($) initial .
  mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1) <$ eAdd
    ]

counterExample1 ::
  MonadWidget t m =>
  Int ->
  m (Dynamic t Int)
counterExample1 i =
  mkCounter (counter1 i)

data Settings =
  Settings {
    settingsLimit :: Int
  , settingsStep :: Int
  }

counter2 ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Dynamic t Settings ->
  Event t () ->
  Event t () ->
  m (Dynamic t Int)
counter2 dSettings eAdd eClear = mdo
  let
    dLimit = settingsLimit <$> dSettings
    dStep = settingsStep <$> dSettings
    dAtLimit = (\c s l -> c + s <= l) <$> dCount <*> dStep <*> dLimit
    eAddOK = gate (current dAtLimit) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+)     <$> tag (current dStep) eAddOK
    , const 0 <$  eClear
    ]

  return dCount

counterExample2 ::
  MonadWidget t m =>
  Dynamic t Int ->
  Dynamic t Int ->
  m (Dynamic t Int)
counterExample2 dLimit dStep =
  mkCounter $ counter2 $ Settings <$> dLimit <*> dStep

bigExample ::
  MonadWidget t m =>
  m ()
bigExample = do
  dLimit <- el "div" $ do
    text "Limit"
    counterExample1 5
  dStep <- el "div" $ do
    text "Step"
    counterExample1 1
  _ <- el "div" $ do
    text "Counter"
    mkCounter $ counter2 $ Settings <$> dLimit <*> dStep
  pure ()
