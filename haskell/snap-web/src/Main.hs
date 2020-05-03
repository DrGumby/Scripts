{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core
import Snap.Http.Server (httpServe, setPort)
import Snap.Util.FileServe (serveFile)
import System.Environment (lookupEnv)
import Snap.Snaplet

showInfo :: MonadSnap m => m ()
showInfo = sendFile "views/index.html"

main :: IO ()
main = do
    mbPort <- lookupEnv "PORT"
    let port = maybe 8000 read mbPort
        config = setPort port mempty
    httpServe config $ 
        ifTop (serveFile "views/index.html")
        <|> route
            [ ("/info", method GET showInfo)
            ]
