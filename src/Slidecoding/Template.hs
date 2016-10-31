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

import Prelude                     hiding (id, head, div)

import Paths_slidecoding                  (getDataFileName)

import Data.String                        (IsString(..), fromString)
import System.Directory                   (copyFile, createDirectoryIfMissing)
import System.FilePath                    ((</>), takeDirectory)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.String   (renderHtml)

distributeAssets :: FilePath -> IO ()
distributeAssets directory = mapM_ (distribute directory) files
  where files = [ defaultIcon
                , highlightJS
                , highlightStyle
                , jquery
                , modernizr
                , deckjsScript
                ] ++ deckjsStylesheets

defaultIcon, highlightJS, highlightStyle :: IsString a => a
defaultIcon    = "lambda.png"
highlightJS    = "highlight-pack-haskell.js"
highlightStyle = "paraiso-dark-min.css"

jquery, modernizr, deckjsScript, deckjsStyle :: IsString a => a
jquery       = "deckjs/jquery.min.js"
modernizr    = "deckjs/modernizr.custom.js"
deckjsScript = "deckjs/core/deck.core.js"
deckjsStyle  = "deckjs/core/deck.core.css"

deckjsStylesheets :: IsString a => [a]
deckjsStylesheets = deckjsStyle : deckjsExtensions ++ deckjsThemes

deckjsExtensions :: IsString a => [a]
deckjsExtensions = [ "deckjs/extensions/goto/deck.goto.css"
                   , "deckjs/extensions/menu/deck.menu.css"
                   , "deckjs/extensions/navigation/deck.navigation.css"
                   , "deckjs/extensions/status/deck.status.css"
                   , "deckjs/extensions/scale/deck.scale.css"
                   ]

deckjsThemes :: IsString a => [a]
deckjsThemes = [ "deckjs/themes/style/web-2.0.css"
               ]

distribute :: FilePath -> FilePath -> IO ()
distribute directory filename = getOriginal >>= copy
  where getOriginal = getDataFileName filename
        copy from   = safeCopyFile from to
        to          = directory </> filename

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = do
  createDirectoryIfMissing True (takeDirectory to)
  copyFile from to

template :: String -> Html -> Html
template titleText slides = do
  docType
  html $ do
    head $ do
      title' titleText
      favicon defaultIcon
      installHighlightJS
      installDeckJS
    body ! class_ "deck-container" $ do
      slides
      bootDeckJS

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Html -> Html
mkSection ""  [] = wrapSection
mkSection id' [] = wrapSection ! id (fromString id')
mkSection ""  cs = wrapSection                       ! class_ (fromString (unwords cs))
mkSection id' cs = wrapSection ! id (fromString id') ! class_ (fromString (unwords cs))

asComment :: String -> Html
asComment = stringComment

installHighlightJS :: Html
installHighlightJS = do
  css highlightStyle
  script' highlightJS
  javascript "hljs.initHighlightingOnLoad();"

installDeckJS :: Html
installDeckJS = do
  mapM_ css deckjsStylesheets
  script' jquery
  script' modernizr
  script' deckjsScript

bootDeckJS :: Html
bootDeckJS = javascript "$(function() { $.deck('.slide'); });"

wrapSection :: Html -> Html
wrapSection = section ! class_ "slide"

favicon, css, script' :: AttributeValue -> Html
favicon file = link ! rel "icon"
                    ! type_ "image/png"
                    ! href file
css     file = link ! rel "stylesheet"
                    ! href file
script' file = script ! src file
                      $ toHtml empty

javascript :: String -> Html
javascript = script . toHtml

title' :: String -> Html
title' = title . toHtml

empty :: String
empty = ""
