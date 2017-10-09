{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( helo
    ) where

import Web.Scotty

helo :: ActionM ()
helo = do
  html "Hello World!"
