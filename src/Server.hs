{-# language OverloadedStrings #-}

module Server
( runServer ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import Network.HTTP.Types.Status (ok200, found302)
import Network.Wai as W
import Web.Firefly

import Util

runServer :: IO String -> IO ()
runServer handler = run 8000 (app handler)

app :: IO String -> App ()
app ioHandler = do
  route "/foo" (fooHandler ioHandler)
  route "/twgl-full.module.js" twglHandler

fooHandler :: IO String -> Handler W.Response
fooHandler ioHandler = do
  s <- liftIO ioHandler
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)

twglHandler :: Handler JS
twglHandler = do
  twgl <- liftIO $ readFile "twgl-full.module.js"
  return (JS twgl)

newtype JS = JS String

instance ToResponse JS where
  toResponse (JS s) =
    toResponse (T.pack s :: T.Text, ok200, M.fromList [("Content-type", ["application/javascript"])] :: HeaderMap)
