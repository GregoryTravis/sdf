{-# language OverloadedStrings #-}

module Server
( runServer ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
-- import Network.HTTP.Types.Status
import Network.Wai as W
import Web.Firefly

import Util

runServer :: IO String -> IO ()
runServer handler = run 8000 (app handler)

app :: IO String -> App ()
app ioHandler = do
  route "/" (indexHandler ioHandler)
  route "/twgl-full.module.js" twglHandler

indexHandler :: IO String -> Handler W.Response
indexHandler ioHandler = do
  s <- liftIO ioHandler
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)

twglHandler :: Handler JS
twglHandler = do
  twgl <- liftIO $ readFile "twgl-full.module.js"
  return (JS twgl)

newtype JS = JS String

-- Found this nastiness in the firefly source
instance ToResponse JS where
  toResponse (JS s) =
    W.responseLBS ok200 [("Content-Type", "application/javascript")] (toLBS $ T.pack s)

toLBS :: T.Text -> LBS.ByteString
toLBS = LT.encodeUtf8 . LT.fromStrict
