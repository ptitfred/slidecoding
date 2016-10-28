{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Template
    ( distributeAssets
    , mkSection
    , template
    , wrapSection
    -- Reexport Text.Blaze utilities:
    , Html
    , renderHtml
    ) where

import Prelude                     hiding (id, head, div)

import Paths_slidecoding                  (getDataFileName)

import Data.String                        (IsString(..), fromString)
import System.Directory                   (copyFile)
import System.FilePath                    ((</>))
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.String   (renderHtml)

distributeAssets :: FilePath -> IO ()
distributeAssets directory = mapM_ (distribute directory) files
  where files = [ defaultIcon
                , baseStylesheet
                , highlightJS
                , highlightStyle
                ]

defaultIcon, baseStylesheet, highlightJS, highlightStyle :: IsString a => a
defaultIcon    = "lambda.png"
baseStylesheet = "slidecoding.css"
highlightJS    = "highlight-pack-haskell.js"
highlightStyle = "paraiso-dark-min.css"

distribute :: FilePath -> FilePath -> IO ()
distribute directory filename = getOriginal >>= copy
  where getOriginal = getDataFileName filename
        copy from   = copyFile from to
        to          = directory </> filename

template :: String -> Html -> Html
template titleText slides =
  html $ do
    head $ do
      title' titleText
      favicon defaultIcon
      css baseStylesheet
      installHighlightJS
    body $
      div ! id (fromString "slides") $ slides

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Html -> Html
mkSection ""  [] = wrapSection
mkSection id' [] = wrapSection ! id (fromString id')
mkSection ""  cs = wrapSection                       ! class_ (fromString (unwords cs))
mkSection id' cs = wrapSection ! id (fromString id') ! class_ (fromString (unwords cs))

installHighlightJS :: Html
installHighlightJS = do
  css highlightStyle
  script' highlightJS
  javascript "hljs.initHighlightingOnLoad();"

wrapSection :: Html -> Html
wrapSection = section

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
