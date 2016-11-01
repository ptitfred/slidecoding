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
import Slidecoding.Types                  (Presentation, meta, design, Design(..), icon, Theme(..))

import Prelude                     hiding (id, head, div)

import Data.Aeson                         (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy.Char8         (unpack)
import Data.Maybe                         (fromMaybe)
import Data.String                        (IsString(..), fromString)
import Text.Blaze.Html5            hiding (object, meta)
import Text.Blaze.Html5.Attributes hiding (title, height, width, icon)
import Text.Blaze.Html.Renderer.String    (renderHtml)

distributeAssets :: Presentation -> IO ()
distributeAssets presentation = distribute presentation (bundle design')
  where design' = design (meta presentation)

bundle :: Design -> Asset
bundle d = favicon d
        <> highlightJS
        <> deckjs d

favicon :: Design -> Asset
favicon = maybe defaultFavicon Favicon . icon

defaultFavicon :: Asset
defaultFavicon = Favicon "lambda.png"

highlightJS :: Asset
highlightJS = JS "highlight-pack-haskell.js"
           <> CSS "paraiso-dark-min.css"
           <> InlineJS "hljs.initHighlightingOnLoad();"

deckjs :: Design -> Asset
deckjs d = -- Dependencies before core otherwise core doesn't boot
           deckjsDependencies
        <> deckjsCore
        <> deckjsTheme (fromMaybe defaultTheme (theme d))
        <> deckjsExtensions

deckjsCore :: Asset
deckjsCore = JS "deckjs/core/deck.core.js" <> CSS "deckjs/core/deck.core.css"

deckjsDependencies :: Asset
deckjsDependencies = jquery <> modernizr
  where jquery     = JS "deckjs/jquery.min.js"
        modernizr  = JS "deckjs/modernizr.custom.js"


defaultTheme :: Theme
defaultTheme = Builtin "web-2.0"

deckjsTheme :: Theme -> Asset
deckjsTheme (Builtin n) = CSS path
  where path = "deckjs/themes/style/" ++ n ++ ".css"
deckjsTheme (Patch t l) = deckjsTheme t <> CSS l
deckjsTheme (Custom l)  = CSS l

deckjsExtensions :: Asset
deckjsExtensions = extensionFit

extensionFit :: Asset
extensionFit = CSS "deckjs/extensions/fit/deck.fit-fs.css"
            <> JS  "deckjs/extensions/fit/deck.fit.js"

template :: Design -> String -> Html -> Html
template d titleText slides = do
  docType
  html $ do
    head $ do
      title' titleText
      include (favicon d)
      include highlightJS
      include (deckjs d)
    body ! class_ "deck-container" $ do
      slides
      include $ bootDeckJS d

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Html -> Html
mkSection ""  cs = section                       ! class_ (fromString (unwords $ "slide" : cs))
mkSection id' cs = section ! id (fromString id') ! class_ (fromString (unwords $ "slide" : cs))

asComment :: String -> Html
asComment = stringComment

newtype Configuration = Configuration Design

instance ToJSON Configuration where
  toJSON (Configuration d) = object [ "designWidth" .= width d, "designHeight" .= height d ]

bootDeckJS :: Design -> Asset
bootDeckJS d = InlineJS $ "$(function() { $.deck('.slide', " ++ encode' d ++ " ); });"
  where encode' = unpack . encode . Configuration

wrapSection :: Html -> Html
wrapSection = mkSection "" []

title' :: String -> Html
title' = title . toHtml
