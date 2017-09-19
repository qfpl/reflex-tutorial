{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Run (
    run
  ) where

import Data.Foldable (traverse_)

import Reflex.Dom.Core

import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle            (JSM)
import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr, jsaddleApp)
import           Network.Wai                            (Application)
import           Network.Wai.Middleware.Static

import           System.FilePath                        ((</>))
import           System.Directory                       (listDirectory)
import qualified Data.Text                              as Text
import qualified Data.Map                               as Map

-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend

  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port

headSection ::
  MonadWidget t m =>
  FilePath ->
  [FilePath] ->
  m ()
headSection cssPath cssFiles =
  let
    stylesheet s =
      elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
        pure ()
  in do
    elAttr "meta" ("charset" =: "utf-8") $
      pure ()
    stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
    traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles

serveCss :: FilePath -> (forall x. Widget x ()) -> IO (Application, JSM ())
serveCss cssPath w = do
  cssFiles <- listDirectory $ "." </> cssPath
  let
    frontendApp =
      mainWidgetWithHead (headSection cssPath cssFiles) w
    backendApp  =
      staticPolicy $ hasPrefix cssPath
  pure (backendApp jsaddleApp, frontendApp)

run' ::
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
run' cssPath port w = do
  (backendApp, frontendApp) <- serveCss cssPath w
  devMainAutoReload backendApp frontendApp port

run ::
  (forall x. Widget x ())
  -> IO ()
run =
  run' "css/exercises" 8080

