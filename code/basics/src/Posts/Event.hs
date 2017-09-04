{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
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

import Util.Reflex
import Util.Number
import Util.Attach
import Util.Grid
import Util.SVG

eventPostExamples ::
  MonadJSM m =>
  m ()
eventPostExamples = do
  attachId_ "basics-events-frame" $
    wrapDemo id mkRedBlueInput
  attachId_ "basics-events-tick"
    demoTick
  attachId_ "basics-events-flipper" $
    wrapDemo flipper mkRedBlueInput
  attachId_ "basics-events-blue" $
    wrapDemo blue mkRedBlueInput
  attachId_ "basics-events-red" $
    wrapDemo red mkRedBlueInput
  attachId_ "basics-events-parse" $
    wrapDemo (parse . fmap unParseText) mkParseInput
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
  mkSquare gc y x c =
    let
      attrs =
        "class" =: "grid-square" <>
        "fill" =: "gray" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

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
  eClick <- buttonClass "btn btn-default" "Go"
  return $ ParseText <$> current (tiColour ^. textInput_value) <@ eClick

wrapDemo ::
  ( MonadWidget t m
  , Square a
  , Square b) =>
  (Event t a -> Event t b) ->
  m (Event t a) ->
  m ()
wrapDemo guest mkIn = divClass "panel panel-default" . divClass "panel-body" $ mdo
  let w = runDemo guest eInput
  _ <- widgetHold w (w <$ eReset)
  (eInput, eReset) <- el "div" $ do
    eInput <- mkIn
    eReset <- buttonClass "btn btn-default pull-right" "Reset"
    return (eInput, eReset)
  return ()

runDemo ::
  ( MonadWidget t m
  , Square a
  , Square b
  ) =>
  (Event t a -> Event t b) ->
  Event t a ->
  m ()
runDemo guest eInput = do

  let
    eOutput =
      guest eInput

  dInputs <- foldDyn (:) [] .
             leftmost $ [
                 Just <$> eInput
               , Nothing <$ eOutput
               ]

  dOutputs <- foldDyn (:) [] .
              leftmost $ [
                  Just <$> eOutput
                , Nothing <$ eInput
                ]

  drawGrid
    defaultGridConfig
    [ Row "eInput" 1 dInputs
    , Row "eOutput" 3 dOutputs
    ]

data UnitSquare = UnitSquare

instance Square UnitSquare where
  mkSquare gc y x c =
    let
      attrs =
        "class" =: "grid-square" <>
        "fill" =: "gray" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

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
demoEither = divClass "panel panel-default" . divClass "panel-body" $ mdo
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
    eReset <- buttonClass "btn btn-default pull-right" "Reset"
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
demoClickMe = divClass "panel panel-default" . divClass "panel-body" $ do
  eClick <- buttonClass "btn btn-default" "Click Me"
  eOnes <- accum (+) 0 (1 <$ eClick)
  let
    eHundreds = (* 100) <$> eOnes
    eSum      = mergeWith (+) [eOnes, eHundreds]
  alertEvent show eSum

demoTick ::
  MonadWidget t m =>
  m ()
demoTick = divClass "panel panel-default" . divClass "panel-body" $ mdo
  let
    wInitial = Workflow $ do
      eNext <- buttonClass "btn btn-default" "Start"
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
    eClick' <- buttonClass "btn btn-default" "Click"
    now <- liftIO getCurrentTime
    eTick' <- tickLossy 2 now
    eReset' <- buttonClass "btn btn-default pull-right" "Reset"
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
  el "row" $ do
    elClass "div" "col-md-6" . el "pre" $
      text label
    elClass "div" "col-md-6" . el "pre" $
      mLabel

wrapPairs ::
  MonadWidget t m =>
  m a ->
  m a
wrapPairs =
  el "container"

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
    eCount = floor <$> _numberInput_input num
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
demoFizzBuzz w = divClass "panel panel-default" . divClass "panel-body" $ mdo
  _ <- widgetHold w (w <$ eReset)
  eReset <- el "div" $
    buttonClass "btn btn-default pull-right" "Reset"
  pure ()
