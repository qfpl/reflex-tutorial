{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Posts.Event (
    eventPostExamples
  ) where

import Data.Monoid ((<>))

import Control.Lens
import Control.Monad.Trans (liftIO)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

import Reflex
import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM(..), JSString, toJSString)

import Colour

import Util.Number
import Util.Attach
import Util.Grid
import qualified Util.Bootstrap as B

eventPostExamples ::
  MonadJSM m =>
  m ()
eventPostExamples = do
  let
    gc = defaultGridConfig
  attachId_ "basics-events-frame" $
    wrapDemo gc id mkRedBlueInput
  attachId_ "basics-events-tick"
    demoTick
  attachId_ "basics-events-flipper" $
    wrapDemo gc flipper mkRedBlueInput
  attachId_ "basics-events-blue" $
    wrapDemo gc blue mkRedBlueInput
  attachId_ "basics-events-red" $
    wrapDemo gc red mkRedBlueInput
  attachId_ "basics-events-parse" $
    wrapDemo gc (parse . fmap unParseText) mkParseInput
  attachId_ "basics-events-either"
    demoEither
  attachId_ "basics-events-clickMe"
    demoClickMe

  attachId_ "basics-events-fizz-and-buzz" $
    demoFizzBuzz fizzAndBuzz
  attachId_ "basics-events-leftmost" $
    demoFizzBuzz fizzBuzzLeftmost
  attachId_ "basics-events-mergeWith" $
    demoFizzBuzz fizzBuzzMergeWith
  attachId_ "basics-events-merge" $
    demoFizzBuzz fizzBuzzMerge
  attachId_ "basics-events-fizzbuzz" $
    demoFizzBuzz fizzBuzz
  attachId_ "basics-events-fizzbuzz-flip" $
    demoFizzBuzz fizzBuzzFlip


flipper :: Reflex t
        => Event t Colour
        -> Event t Colour
flipper eInput =
  let
    eOutput =
      flipColour <$> eInput
  in
    eOutput

blue :: Reflex t
     => Event t Colour
     -> Event t Colour
blue eInput =
  let
    eOutput =
      Blue <$ eInput
  in
    eOutput

isRed :: Colour
      -> Bool
isRed Red =
  True
isRed Blue =
  False

red :: Reflex t
    => Event t Colour
    -> Event t Colour
red eInput =
  let
    eOutput =
      ffilter isRed eInput
  in
    eOutput

newtype ParseText = ParseText { unParseText :: Text }

instance Square ParseText where
  mkSquareAttrs gc y x da =
    let
      mkAttr Nothing =
        "fill" =: "none" <>
        "stroke" =: "none"
      mkAttr (Just _) =
        "fill" =: "gray" <>
        "class" =: "grid-square"
    in
      (mkAttr <$> da) <> pure (standardAttrs gc y x)

parseColour :: Text
            -> Maybe Colour
parseColour t =
  case t of
    "Red"  -> Just Red
    "Blue" -> Just Blue
    _ -> Nothing

parse :: Reflex t
      => Event t Text
      -> Event t Colour
parse eInput =
  let
    eOutput =
      fmapMaybe parseColour eInput
  in
    eOutput

mkParseInput :: MonadWidget t m
             => m (Event t ParseText)
mkParseInput = do
  tiColour <- textInput $ def & textInputConfig_initialValue .~ "Blue"
  eClick <- B.button "Go"
  return $ ParseText <$> current (tiColour ^. textInput_value) <@ eClick

wrapDemo ::
  ( MonadWidget t m
  , Square a
  , Square b) =>
  GridConfig ->
  (Event t a -> Event t b) ->
  m (Event t a) ->
  m ()
wrapDemo gc guest mkIn = B.panel $ mdo
  let w = runDemo gc guest eInput
  _ <- widgetHold w (w <$ eReset)
  (eInput, eReset) <- el "div" $ do
    eInput <- mkIn
    eReset <- B.buttonClass "pull-right" "Reset"
    return (eInput, eReset)
  return ()

runDemo ::
  ( MonadWidget t m
  , Square a
  , Square b
  ) =>
  GridConfig ->
  (Event t a -> Event t b) ->
  Event t a ->
  m ()
runDemo gc guest eInput = do

  let
    eOutput =
      guest eInput
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

data UnitSquare = UnitSquare

instance Square UnitSquare where
  mkSquareAttrs gc y x da =
    let
      mkAttr Nothing =
        "fill" =: "none" <>
        "stroke" =: "none"
      mkAttr (Just _) =
        "fill" =: "gray" <>
        "class" =: "grid-square"
    in
      (mkAttr <$> da) <> pure (standardAttrs gc y x)

splitColour :: Colour
            -> Either () ()
splitColour Red =
  Left ()
splitColour Blue =
  Right ()

fanDemo :: Reflex t
        => Event t Colour
        -> (Event t (), Event t ())
fanDemo eInput =
  let
    (eLeft, eRight) =
      fanEither .
      fmap splitColour $
      eInput
  in
    (eLeft, eRight)

demoEither ::
  MonadWidget t m =>
  m ()
demoEither = B.panel $ mdo
  let
    widget = do
      let
        (eLeft, eRight) = fanDemo eInput

      dInputs <- foldDyn (:) [] .
                leftmost $ [
                    Just <$> eInput
                  , Nothing <$ eLeft
                  , Nothing <$ eRight
                  ]

      dLefts <- foldDyn (:) [] .
                  leftmost $ [
                      Just UnitSquare <$ eLeft
                    , Nothing <$ eInput
                    , Nothing <$ eRight
                    ]

      dRights <- foldDyn (:) [] .
                  leftmost $ [
                      Just UnitSquare <$ eRight
                    , Nothing <$ eInput
                    , Nothing <$ eLeft
                    ]

      drawGrid
        (defaultGridConfig & gcRows .~ 7)
        [ Row "eInput" 1 dInputs
        , Row "eLeft" 3 dLefts
        , Row "eRight" 5 dRights
        ]

  _ <- widgetHold widget (widget <$ eReset)
  (eInput, eReset) <- el "div" $ do
    eInput <- mkRedBlueInput
    eReset <- B.buttonClass "pull-right" "Reset"
    return (eInput, eReset)
  return ()

alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()
#ifdef ghcjs_HOST_OS
alertEvent str e = performEvent_ (alert <$> e)
  where
    alert a = liftIO $ js_alert $ toJSString $ str a

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> IO ()
#else
alertEvent = error "alertEvent: can only be used with GHCJS"
js_alert = error "js_alert: can only be used with GHCJS"
#endif

demoClickMe ::
  MonadWidget t m =>
  m ()
demoClickMe = B.panel $ do
  eClick <- B.button "Click Me"
  eOnes <- accum (+) (0 :: Int) (1 <$ eClick)
  let
    eHundreds = (* 100) <$> eOnes
    eSum      = mergeWith (+) [eOnes, eHundreds]
  alertEvent show eSum

demoTick ::
  MonadWidget t m =>
  m ()
demoTick = B.panel $ mdo
  let
    wInitial = Workflow $ do
      eNext <- B.button "Start"
      pure ((), wRunning <$ eNext)
    wRunning = Workflow $ do
      eStop <- demoTick'
      pure ((), wInitial <$ eStop)

  _ <- workflow wInitial
  pure ()

demoTick' ::
  MonadWidget t m =>
  m (Event t ())
demoTick' = mdo
  drawGrid defaultGridConfig
    [ Row "eClick" 1 dClick
    , Row "eTick" 3 dTick
    ]

  (eClick, eTick, eReset) <- el "div" $ do
    eClick' <- B.button "Click"
    now <- liftIO getCurrentTime
    eTick' <- tickLossy 2 now
    eReset' <- B.buttonClass "pull-right" "Reset"
    return (Blue <$ eClick', Red <$ eTick', eReset')

  dClick <- foldDyn (:) [] .
            leftmost $ [
                Just <$> eClick
              , Nothing <$ eTick
              ]

  dTick <- foldDyn (:) [] .
           leftmost $ [
               Just <$> eTick
             , Nothing <$ eClick
             ]

  return eReset

layoutPair ::
  MonadWidget t m =>
  Text ->
  m a ->
  m a
layoutPair label mLabel = do
  el "tr" $ do
    el "td" . el "pre" $
      text label
    el "td" . el "pre" $
      mLabel

wrapPairs ::
  MonadWidget t m =>
  m a ->
  m a
wrapPairs =
  el "table"

fizzBuzzPart ::
  MonadWidget t m =>
  Event t () ->
  Event t Text ->
  Text ->
  m ()
fizzBuzzPart eTick eLabel label = do
  dLabel <- holdDyn "" .
            leftmost $
            [ eLabel , "" <$ eTick]
  layoutPair label (dynText dLabel)

fizzAndBuzzBits ::
  MonadWidget t m =>
  m (Event t Int, Event t Text, Event t Text)
fizzAndBuzzBits = do
  num <- layoutPair "eCount" $
    numberInput def

  let
    eCount = _numberInput_input num
    eTick = () <$ eCount

  let
    eFizz = "Fizz" <$ ffilter ((== 0) . (`mod` 3)) eCount

  fizzBuzzPart eTick eFizz "eFizz"

  let
    eBuzz = "Buzz" <$ ffilter ((== 0) . (`mod` 5)) eCount

  fizzBuzzPart eTick eBuzz "eBuzz"

  pure (eCount, eFizz, eBuzz)

fizzAndBuzz ::
  MonadWidget t m =>
  m ()
fizzAndBuzz = wrapPairs $ do
  _ <- fizzAndBuzzBits
  pure ()

fizzBuzzLeftmost ::
  MonadWidget t m =>
  m ()
fizzBuzzLeftmost = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount
    eLeft = leftmost [eFizz, eBuzz]

  fizzBuzzPart eTick eLeft "eLeft"

fizzBuzzMergeWith ::
  MonadWidget t m =>
  m ()
fizzBuzzMergeWith = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount
    eMerge = mergeWith (<>) [eFizz, eBuzz]

  fizzBuzzPart eTick eMerge "eMergeWith"

fizzBuzzMerge ::
  MonadWidget t m =>
  m ()
fizzBuzzMerge = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount

  let
    eMerge      = eFizz <> eBuzz

  fizzBuzzPart eTick eMerge "eMerge"

fizzBuzz ::
  MonadWidget t m =>
  m ()
fizzBuzz = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eCountText = (Text.pack . show) <$> eCount
    eTick = () <$ eCount

  let
    eFizzBuzz = leftmost [eFizz <> eBuzz, eCountText]

  fizzBuzzPart eTick eFizzBuzz "eFizzBuzz"

fizzBuzzFlip ::
  MonadWidget t m =>
  m ()
fizzBuzzFlip = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eCountText = (Text.pack . show) <$> eCount
    eTick = () <$ eCount

  let
    eMerge      = eFizz <> eBuzz

  fizzBuzzPart eTick eMerge "eMerge"

  let
    eDiff = difference eCountText eMerge

  fizzBuzzPart eTick eDiff "eDiff"

  let
    eFizzBuzzFlip = leftmost [eDiff, eMerge]

  fizzBuzzPart eTick eFizzBuzzFlip "eFizzBuzz"

demoFizzBuzz ::
  MonadWidget t m =>
  m () ->
  m ()
demoFizzBuzz w = B.panel $ mdo
  _ <- widgetHold w (w <$ eReset)
  eReset <- el "div" $
    B.buttonClass "pull-right" "Reset"
  pure ()
