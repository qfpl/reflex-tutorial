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
import           Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr, jsaddleOr, jsaddleApp)
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

serveCss :: FilePath -> (forall x. Widget x ()) -> IO (Application, JSM ())
serveCss cssPath w = do
  cssFiles <- listDirectory $ "." </> cssPath
  let
    frontendApp = do
      let
        stylesheet s =
          elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
            return ()
      mainWidgetWithHead
        (traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles)
        w
    backendApp  = 
      staticPolicy $ hasPrefix cssPath
  pure (backendApp jsaddleApp, frontendApp)

run2' ::
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
run2' cssPath port w = do
  (backendApp, frontendApp) <- serveCss cssPath w
  devMainAutoReload backendApp frontendApp port

run1' ::
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
run1' cssPath port w =
  do
    cssFiles <- listDirectory $ "." </> cssPath
    let
      f = do
        let
          stylesheet s =
            elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
              return ()
        mainWidgetWithHead
          (traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles)
          w
      serveFiles = staticPolicy $ hasPrefix cssPath

    debugWrapper $ \refreshMiddleware registerContext -> do
      app <- jsaddleOr 
               defaultConnectionOptions 
               (registerContext >> f >> syncPoint) 
               (refreshMiddleware jsaddleApp)
      runSettings (setPort port (setTimeout 3600 defaultSettings)) $
        serveFiles app

run ::
  (forall x. Widget x ())
  -> IO ()
run =
  run2' "css" 8080

