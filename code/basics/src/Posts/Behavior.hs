{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Posts.Behavior (
    behaviorPostExamples
  ) where

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Control.Lens

import Colour

import Util.Reflex
import Util.Attach
import Util.Grid
import qualified Util.Bootstrap as B

behaviorPostExamples ::
  MonadJSM m =>
  m ()
behaviorPostExamples = do
  let
    gc = defaultGridConfig

  attachId_ "basics-behaviors-sample" $
    wrapDemo gc sampleBlue mkRedBlueInput
  attachId_ "basics-behaviors-sampleBlue1" $
    wrapDemo gc sampleBlue1 mkRedBlueInput
  attachId_ "basics-behaviors-sampleBlue2" $
    wrapDemo gc sampleBlue2 mkRedBlueInput

  mbGate <- attachId "basics-behaviors-gateOut"
    gateOut
  case mbGate of
    Nothing -> pure ()
    Just bGate -> attachId_ "basics-behaviors-gateIn" $
      gateIn gc bGate

  attachId_ "basics-behaviors-sampleFlipBlue" $
    wrapDemo gc sampleFlipBlue mkRedBlueInput
  attachId_ "basics-behaviors-sampleAlwaysBlue" $
    wrapDemo gc (const $ pure . sampleAlwaysBlue) mkRedBlueInput
  attachId_ "basics-behaviors-samplePair" $
    wrapDemo2 gc samplePair mkRedBlueInput

sampleBlue :: (Reflex t, MonadHold t m)
            => Event t Colour
            -> Event t ()
            -> m (Event t Colour)
sampleBlue eColour eSample = do
  bColour <- hold Blue eColour
  colour <- sample bColour
  pure $ colour <$ eSample

sampleBlue1 :: (Reflex t, MonadHold t m)
            => Event t Colour
            -> Event t ()
            -> m (Event t Colour)
sampleBlue1 eColour eSample = do
  bColour <- hold Blue eColour
  pure $ tag bColour eSample

sampleBlue2 :: (Reflex t, MonadHold t m)
            => Event t Colour
            -> Event t ()
            -> m (Event t Colour)
sampleBlue2 eColour eSample = do
  bColour <- hold Blue eColour
  let eAny = leftmost [() <$ eColour, eSample]
  pure $ tag bColour eAny

sampleFlipBlue :: (Reflex t, MonadHold t m)
               => Event t Colour
               -> Event t ()
               -> m (Event t Colour)
sampleFlipBlue eColour eSample = do
  bColour <- hold Blue eColour
  let bFlippedColour = flipColour <$> bColour
  pure $ tag bFlippedColour eSample

sampleAlwaysBlue :: Reflex t
                 => Event t ()
                 -> Event t Colour
sampleAlwaysBlue eSample =
  tag (pure Blue) eSample

samplePair :: (Reflex t, MonadHold t m)
           => Event t Colour
           -> Event t Colour
           -> Event t ()
           -> m (Event t (Colour, Colour))
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2
  let bPair = (,) <$> bColour1 <*> bColour2
  pure $ tag bPair eSample

wrapDemo ::
  ( MonadWidget t m
  , Square a
  , Square b) =>
  GridConfig ->
  (Event t a -> Event t () -> m (Event t b)) ->
  m (Event t a) ->
  m ()
wrapDemo gc guest mkIn = B.panel $ mdo
  let w = runDemo gc guest eInput eSample
  _ <- widgetHold w (w <$ eReset)
  (eInput, eSample, eReset) <- el "div" $ do
    eInput <- mkIn
    eSample <- B.button "Sample"
    eReset <- B.buttonClass "pull-right" "Reset"
    return (eInput, eSample, eReset)
  return ()

gateOut ::
  MonadWidget t m =>
  m (Behavior t Bool)
gateOut = B.panel $ do
  text "Allow events to pass through"

  cb <- checkbox True def
  pure . current $ cb ^. checkbox_value

gateIn ::
  MonadWidget t m =>
  GridConfig ->
  Behavior t Bool ->
  m ()
gateIn gc bGate = B.panel $ mdo
    let w = runDemo gc (\e _ -> pure e) eInput never
    _ <- widgetHold w (w <$ eReset)
    (eInput, eReset) <- el "div" $ do
      eInput <- mkRedBlueInput
      eReset <- B.buttonClass "pull-right" "Reset"
      return (gate bGate eInput, eReset)
    pure ()

runDemo ::
  ( MonadWidget t m
  , Square a
  , Square b
  ) =>
  GridConfig ->
  (Event t a -> Event t () -> m (Event t b)) ->
  Event t a ->
  Event t () ->
  m ()
runDemo gc guest eInput eSample = do
  eOutput <- guest eInput eSample

  let
    acc :: c -> [c] -> [c]
    acc x xs =
      take (_gcColumns gc) (x : xs)

  dInputs <- foldDyn acc [] .
             leftmost $ [
                 Just <$> eInput
               , Nothing <$ eOutput
               ]

  dOutputs <- foldDyn acc [] .
              leftmost $ [
                  Just <$> eOutput
                , Nothing <$ eInput
                ]

  drawGrid gc
    [ Row "eInput" 1 dInputs
    , Row "eOutput" 3 dOutputs
    ]

wrapDemo2 ::
  ( MonadWidget t m
  ) =>
  GridConfig ->
  (Event t Colour -> Event t Colour -> Event t () -> m (Event t (Colour, Colour))) ->
  m (Event t Colour) ->
  m ()
wrapDemo2 gc guest mkIn = B.panel $ mdo
  let w = runDemo2 gc guest eInput1 eInput2 eSample
  _ <- widgetHold w (w <$ eReset)
  (eInput1, eInput2, eSample, eReset) <- el "div" $ do
    eInput1 <- mkIn
    eInput2 <- mkIn
    eSample <- B.button "Sample"
    eReset <- B.buttonClass "pull-right" "Reset"
    return (eInput1, eInput2, eSample, eReset)
  return ()

runDemo2 ::
  ( MonadWidget t m
  ) =>
  GridConfig ->
  (Event t Colour -> Event t Colour -> Event t () -> m (Event t (Colour, Colour))) ->
  Event t Colour ->
  Event t Colour ->
  Event t () ->
  m ()
runDemo2 gc guest eInput1 eInput2 eSample = do
  eOutput <- guest eInput1 eInput2 eSample

  let
    acc :: c -> [c] -> [c]
    acc x xs =
      take (_gcColumns gc) (x : xs)

  dInput1 <- foldDyn acc [] .
             leftmost $ [
                 Just <$> eInput1
               , Nothing <$ eInput2
               , Nothing <$ eOutput
               ]

  dInput2 <- foldDyn acc [] .
             leftmost $ [
                 Just <$> eInput2
               , Nothing <$ eInput1
               , Nothing <$ eOutput
               ]

  dOutputs <- foldDyn acc [] .
              leftmost $ [
                  Just <$> eOutput
                , Nothing <$ eInput1
                , Nothing <$ eInput2
                ]

  drawGrid
    (defaultGridConfig { _gcRows = 7})
    [ Row "eInput1" 1 dInput1
    , Row "eInput2" 3 dInput2
    , Row "eOutput" 5 dOutputs
    ]
