{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Web.Scotty as Scotty

main :: IO ()
main =
  Scotty.scotty 8080 $ do
    Scotty.get "/" indexPage

