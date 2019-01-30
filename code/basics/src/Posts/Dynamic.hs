{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Dynamic (
    dynamicPostExamples
  ) where

import Control.Monad.Fix (MonadFix)

import Control.Lens

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Colour

import Util.Attach
import Util.Grid
import qualified Util.Bootstrap as B

dynamicPostExamples ::
  MonadJSM m =>
  m ()
dynamicPostExamples = do
  attachId_ "basics-dynamic-counter-1"
    counterExample1
  attachId_ "basics-dynamic-counter-2"
    counterExample2
  attachId_ "basics-dynamic-split-1" $
    wrapDemo2 splitDemo1 mkRedBlueInput
  attachId_ "basics-dynamic-split-2" $
    wrapDemo2 splitDemo2 mkRedBlueInput

counterExample1 ::
  MonadWidget t m =>
  m ()
counterExample1 = B.panel $ mdo
  el "div" $
    display dCount

  eAdd <- el "span" $
    B.button "Add"

  dCount <- foldDyn ($) (0 :: Int) $
      (+ 1) <$ eAdd

  pure ()

counterExample2 ::
  MonadWidget t m =>
  m ()
counterExample2 = B.panel $ mdo
  el "div" $
    display dCount

  (eAdd, eClear) <- el "span" $ do
    eAdd'   <- B.button "Add"
    eClear' <- B.button "Clear"
    pure (eAdd', eClear')

  dCount <- foldDyn ($) (0 :: Int) . mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1) <$ eAdd
    ]

  pure ()

dynPair :: (Reflex t, MonadHold t m)
        => Event t Colour
        -> Event t Colour
        -> m (Dynamic  t (Colour, Colour))
dynPair eInput1 eInput2 = do
  dColour1 <- holdDyn Blue eInput1
  dColour2 <- holdDyn Blue eInput2
  pure $ (,) <$> dColour1 <*> dColour2

splitPair1 :: Reflex t
           => Dynamic t (Colour, Colour)
           -> (Dynamic t Colour, Dynamic t Colour)
splitPair1 dPair =
  let
    p1 = fmap fst dPair
    p2 = fmap snd dPair
  in
    (p1, p2)

splitDemo1 :: (Reflex t, MonadHold t m)
           => Event t Colour
           -> Event t Colour
           -> m (Event t Colour, Event t Colour)
splitDemo1 e1 e2 = do
  dPair <- dynPair e1 e2
  let (od1, od2) = splitPair1 dPair
  pure (updated od1, updated od2)

splitPair2 :: (Reflex t, MonadHold t m, MonadFix m)
           => Dynamic t (Colour, Colour)
           -> m (Dynamic t Colour, Dynamic t Colour)
splitPair2 dPair =
  do
    p1 <- holdUniqDyn (fmap fst dPair)
    p2 <- holdUniqDyn (fmap snd dPair)
    pure (p1, p2)

splitDemo2 :: (Reflex t, MonadFix m, MonadHold t m)
           => Event t Colour
           -> Event t Colour
           -> m (Event t Colour, Event t Colour)
splitDemo2 e1 e2 = do
  dPair <- dynPair e1 e2
  (od1, od2) <- splitPair2 dPair
  pure (updated od1, updated od2)

wrapDemo2 ::
  ( MonadWidget t m
  , Square a
  ) =>
  (Event t a -> Event t a -> m (Event t a, Event t a)) ->
  m (Event t a) ->
  m ()
wrapDemo2 guest mkIn = B.panel $ mdo
  let w = runDemo2 guest eInput1 eInput2
  _ <- widgetHold w (w <$ eReset)
  (eInput1, eInput2, eReset) <- el "div" $ do
    eInput1' <- mkIn
    eInput2' <- mkIn
    eReset' <- B.buttonClass "pull-right" "Reset"
    return (eInput1', eInput2', eReset')
  return ()

runDemo2 ::
  ( MonadWidget t m
  , Square a
  ) =>
  (Event t a -> Event t a -> m (Event t a, Event t a)) ->
  Event t a ->
  Event t a ->
  m ()
runDemo2 guest eInput1 eInput2 = do
  (eOutput1, eOutput2) <- guest eInput1 eInput2

  dInput1 <- foldDyn (:) [] .
             leftmost $ [
                 Just <$> eInput1
               , Nothing <$ eInput2
               , Nothing <$ eOutput1
               , Nothing <$ eOutput2
               ]

  dInput2 <- foldDyn (:) [] .
             leftmost $ [
                 Just <$> eInput2
               , Nothing <$ eInput1
               , Nothing <$ eOutput1
               , Nothing <$ eOutput2
               ]

  dOutput1 <- foldDyn (:) [] .
              leftmost $ [
                  Just <$> eOutput1
                , Nothing <$ eOutput2
                , Nothing <$ eInput1
                , Nothing <$ eInput2
                ]

  dOutput2 <- foldDyn (:) [] .
              leftmost $ [
                  Just <$> eOutput2
                , Nothing <$ eOutput1
                , Nothing <$ eInput1
                , Nothing <$ eInput2
                ]

  drawGrid
    (defaultGridConfig & gcRows .~ 9 & gcTextWidth .~ 200)
    [ Row "eInput1" 1 dInput1
    , Row "eInput2" 3 dInput2
    , Row "eOutput1" 5 dOutput1
    , Row "eOutput2" 7 dOutput2
    ]
