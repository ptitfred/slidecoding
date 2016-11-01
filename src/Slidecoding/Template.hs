{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Template
    ( Configuration(..)
    , Theme(..)
    , distributeAssets
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

import Data.Aeson                         (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy.Char8         (unpack)
import Data.String                        (IsString(..), fromString)
import Text.Blaze.Html5            hiding (object)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.String    (renderHtml)

distributeAssets :: Configuration -> FilePath -> IO ()
distributeAssets cfg directory = distribute directory bundle
  where bundle = defaultFavicon <> highlightJS <> deckjs cfg

defaultFavicon :: Asset
defaultFavicon = Favicon "lambda.png"

highlightJS :: Asset
highlightJS = JS "highlight-pack-haskell.js"
           <> CSS "paraiso-dark-min.css"
           <> InlineJS "hljs.initHighlightingOnLoad();"

deckjs :: Configuration -> Asset
deckjs cfg = -- Dependencies before core otherwise core doesn't boot
             deckjsDependencies
          <> deckjsCore
          <> deckjsTheme (theme cfg)
          <> deckjsExtensions

deckjsCore :: Asset
deckjsCore = JS "deckjs/core/deck.core.js" <> CSS "deckjs/core/deck.core.css"

deckjsDependencies :: Asset
deckjsDependencies = jquery <> modernizr
  where jquery     = JS "deckjs/jquery.min.js"
        modernizr  = JS "deckjs/modernizr.custom.js"

deckjsTheme :: Theme -> Asset
deckjsTheme Neon  = CSS "deckjs/themes/style/neon.css"
deckjsTheme Swiss = CSS "deckjs/themes/style/swiss.css"
deckjsTheme Web20 = CSS "deckjs/themes/style/web-2.0.css"

deckjsExtensions :: Asset
deckjsExtensions = extensionFit

extensionFit :: Asset
extensionFit = CSS "deckjs/extensions/fit/deck.fit-fs.css"
            <> JS  "deckjs/extensions/fit/deck.fit.js"

template :: Configuration -> String -> Html -> Html
template cfg titleText slides = do
  docType
  html $ do
    head $ do
      title' titleText
      include defaultFavicon
      include highlightJS
      include (deckjs cfg)
    body ! class_ "deck-container" $ do
      slides
      include $ bootDeckJS cfg

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Html -> Html
mkSection ""  cs = section                       ! class_ (fromString (unwords $ "slide" : cs))
mkSection id' cs = section ! id (fromString id') ! class_ (fromString (unwords $ "slide" : cs))

asComment :: String -> Html
asComment = stringComment

type Pixel = Int
data Theme = Neon | Web20 | Swiss

data Configuration = Configuration { designWidth  :: Pixel
                                   , designHeight :: Pixel
                                   , theme        :: Theme
                                   }

instance ToJSON Configuration where
  toJSON cfg = object [ "designWidth" .= designWidth cfg, "designHeight" .= designHeight cfg ]

bootDeckJS :: Configuration -> Asset
bootDeckJS cfg = InlineJS $ "$(function() { $.deck('.slide', " ++ encode' cfg ++ " ); });"
  where encode' = unpack . encode

wrapSection :: Html -> Html
wrapSection = mkSection "" []

title' :: String -> Html
title' = title . toHtml
