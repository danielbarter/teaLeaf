{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Char8 (unpack)
import System.Process (callProcess)


app :: Application
app request respond = do
  let queryString = "tealeaf::" <> ( unpack $ rawQueryString request )
  callProcess "./generateTeaLeaf.elf" [queryString]
  callProcess "convert" ["tealeaf.bmp","tealeaf.png"]
  teaLeaf <- B.readFile "tealeaf.png"
  respond $ responseLBS
    status200 [("Content-Type", "image/png")] teaLeaf

main :: IO ()
main = do
    run 80 app
