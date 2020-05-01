{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Char8 (unpack)
import System.Process (callProcess)
import System.Posix.Files
import System.Exit


app :: Application
app request respond = do
  size <- fromIntegral . fileSize <$> getFileStatus "log"
  if size > 10000
    then exitFailure
    else do appendFile "log" $ ( show $ remoteHost request ) <> "\n"
            let queryString = "tealeaf::" <> ( unpack $ rawQueryString request )
            callProcess "./generateTeaLeaf.elf" [queryString]
            callProcess "convert" ["tealeaf.bmp","tealeaf.png"]
            teaLeaf <- B.readFile "tealeaf.png"
            respond $ responseLBS
              status200 [("Content-Type", "image/png")] teaLeaf

main :: IO ()
main = do
    run 80 app
