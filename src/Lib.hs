{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( indexPage
    ) where

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

indexPage :: S.ActionM ()
indexPage = do
  S.html . renderHtml $ do
    H.h1 "Simple db test"
