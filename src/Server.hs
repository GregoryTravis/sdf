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

import Infinity
import Interactive
import Random
import Single
import Util

runServer :: IO ()
runServer = run 8000 app

app :: App ()
app = do
  route "/" (htmlHandler (crecipes >>= singleHandler))
  route "/m" (htmlHandler (mouseCircleE >>= singleHandler))
  route "/rr" (htmlHandler (realRandom >>= singleHandler))
  route "/rro" (htmlHandler (realRandomOsc >>= singleHandler))
  route "/rrp" (htmlHandler (realRandomPile >>= singleHandler))
  route "/function" (htmlHandler getShapeFunction)
  route "/twgl-full.module.js" twglHandler
  route "/infinity.html" $ htmlFileHandler "infinity.html"
  route "/infinity-main.glsl" $ htmlFileHandler "infinity-main.glsl"

htmlHandler :: IO String -> Handler W.Response
htmlHandler handler = do
  s <- liftIO handler
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)

plainFileHandler :: FilePath -> Handler W.Response
plainFileHandler file = do
  s <- liftIO $ readFile file
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/plain"])] :: HeaderMap)

htmlFileHandler :: FilePath -> Handler W.Response
htmlFileHandler file = do
  html <- liftIO $ readFile file
  return $ toResponse (T.pack html, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)

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
