{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Template
    ( distributeAssets
    , mkSection
    , template
    , wrapSection
    , asComment
    -- Reexport Text.Blaze utilities:
    , Html
    , renderHtml
    ) where

import Slidecoding.Assets

import Prelude                     hiding (id, head, div)

import Data.String                        (IsString(..), fromString)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.String    (renderHtml)

distributeAssets :: FilePath -> IO ()
distributeAssets directory = distribute directory bundle
  where bundle = defaultFavicon <> highlightJS <> deckjs

defaultFavicon :: Asset
defaultFavicon = Favicon "lambda.png"

highlightJS :: Asset
highlightJS = JS "highlight-pack-haskell.js"
           <> CSS "paraiso-dark-min.css"
           <> InlineJS "hljs.initHighlightingOnLoad();"

deckjs :: Asset
deckjs = -- Dependencies before core otherwise core doesn't boot
         deckjsDependencies
      <> deckjsCore
      <> deckjsTheme

deckjsCore :: Asset
deckjsCore = JS "deckjs/core/deck.core.js" <> CSS "deckjs/core/deck.core.css"

deckjsDependencies :: Asset
deckjsDependencies = jquery <> modernizr
  where jquery     = JS "deckjs/jquery.min.js"
        modernizr  = JS "deckjs/modernizr.custom.js"

deckjsTheme :: Asset
deckjsTheme = CSS "deckjs/themes/style/web-2.0.css"

template :: String -> Html -> Html
template titleText slides = do
  docType
  html $ do
    head $ do
      title' titleText
      include defaultFavicon
      include highlightJS
      include deckjs
    body ! class_ "deck-container" $ do
      slides
      include bootDeckJS

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Html -> Html
mkSection ""  [] = wrapSection
mkSection id' [] = wrapSection ! id (fromString id')
mkSection ""  cs = wrapSection                       ! class_ (fromString (unwords cs))
mkSection id' cs = wrapSection ! id (fromString id') ! class_ (fromString (unwords cs))

asComment :: String -> Html
asComment = stringComment

bootDeckJS :: Asset
bootDeckJS = InlineJS "$(function() { $.deck('.slide'); });"

wrapSection :: Html -> Html
wrapSection = section ! class_ "slide"

title' :: String -> Html
title' = title . toHtml
