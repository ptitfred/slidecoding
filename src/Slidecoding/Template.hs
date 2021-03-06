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
import Slidecoding.Types                  (Presentation, rootDir, distDir, metadata, design, Design(..), icon, Theme(..), TransitionStyle(..), Port)

import Prelude                     hiding (id, head, div)

import Control.Monad                      (when)
import Data.Aeson                         (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy.Char8         (unpack)
import Data.Maybe                         (fromMaybe)
import Data.String                        (IsString(..), fromString)
import System.Directory                   (copyFile, createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath                    ((</>))
import Text.Blaze.Html5            hiding (object)
import Text.Blaze.Html5.Attributes hiding (title, height, width, icon)
import Text.Blaze.Html.Renderer.String    (renderHtml)

distributeAssets :: Presentation -> IO ()
distributeAssets presentation = do
  distribute presentation (bundle design')
  copyDirectory (rootDir presentation </> "assets") (distDir presentation </> "assets")
    where design' = design (metadata presentation)

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
  isDirectory <- doesDirectoryExist from
  when isDirectory (copyDirectoryContents from to)

copyDirectoryContents :: FilePath -> FilePath -> IO ()
copyDirectoryContents from to = getDirectoryContents from >>= mapM_ (copyElement from to)

copyElement :: FilePath -> FilePath -> FilePath -> IO ()
copyElement    _  _ "."  = return ()
copyElement    _  _ ".." = return ()
copyElement from to f    = do
  let original = from </> f
  let copy     = to   </> f
  isDirectory <- doesDirectoryExist original
  if isDirectory
  then createDirectoryIfMissing True copy >> copyDirectoryContents original copy
  else copyFile original copy

bundle :: Design -> Asset
bundle d = mainStylesheet
        <> favicon d
        <> highlightJS
        <> deckjs d

mainStylesheet :: Asset
mainStylesheet = CSS "slidecoding.css"

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
        <> deckjsTransition (transition d)
        <> deckjsExtensions

deckjsCore :: Asset
deckjsCore = JS "deckjs/core/deck.core.js"
          <> CSS "deckjs/core/deck.core.css"
          <> CSSPrint "deckjs/core/print.css"

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
deckjsExtensions = deckjsFitExtension <> deckjsReplExtension <> deckjsLichessExtension

deckjsFitExtension :: Asset
deckjsFitExtension = CSS "deckjs/extensions/fit/deck.fit-fs.css"
                  <> JS  "deckjs/extensions/fit/deck.fit.js"

deckjsReplExtension :: Asset
deckjsReplExtension = CSS "deckjs/extensions/repl/deck.repl.css"
                   <> JS  "deckjs/extensions/repl/deck.repl.js"
                   <> JS  "jqconsole.min.js"

deckjsLichessExtension :: Asset
deckjsLichessExtension = JS "deckjs/extensions/lichess/deck.lichess.js"

deckjsTransition :: Maybe TransitionStyle -> Asset
deckjsTransition = maybe mempty deckjsTransition'

deckjsTransition' :: TransitionStyle -> Asset
deckjsTransition' HorizontalSlide = CSS "deckjs/themes/transition/horizontal-slide.css"
deckjsTransition' VerticalSlide   = CSS "deckjs/themes/transition/vertical-slide.css"
deckjsTransition' Fade            = CSS "deckjs/themes/transition/fade.css"

template :: Maybe Port -> Design -> String -> Html -> Html
template port d titleText slides = do
  docType
  html $ do
    head $ do
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
      title' titleText
      include (favicon d)
      include mainStylesheet
      include highlightJS
      include (deckjs d)
    body ! class_ "deck-container" $ do
      slides
      include $ bootDeckJS port d

mkSection :: String   -- id of the section element
          -> [String] -- classes of the section element
          -> Maybe String -- optional REPL context name to store on the section element
          -> Html -> Html
mkSection ""  cs ctx = mkSectionWithContext cs ctx
mkSection id' cs ctx = mkSectionWithContext cs ctx ! id (fromString id')

mkSectionWithContext :: [String] -> Maybe String -> Html -> Html
mkSectionWithContext cs  Nothing           = mkSimpleSection cs
mkSectionWithContext cs (Just replContext) = mkSimpleSection cs ! dataAttribute "repl-context" (fromString replContext)

mkSimpleSection :: [String] -> Html -> Html
mkSimpleSection cs = section ! class_ (fromString (unwords $ "slide" : cs))

asComment :: String -> Html
asComment = stringComment

data Configuration = Configuration (Maybe Port) Design

instance ToJSON Configuration where
  toJSON (Configuration  Nothing d) = object [ "designWidth" .= width d, "designHeight" .= height d ]
  toJSON (Configuration (Just port) d) =
    object [ "repl" .= object [ "endpoint" .= url ]
           , "designWidth"   .= width d
           , "designHeight"  .= height d
           ]
      where url = "//127.0.0.1:" ++ show port ++ "/"

bootDeckJS :: Maybe Port -> Design -> Asset
bootDeckJS port d = InlineJS $ "$(function() { $.deck('.slide', " ++ encode' (Configuration port d) ++ " ); });"
  where encode' = unpack . encode

wrapSection :: Maybe String -> Html -> Html
wrapSection = mkSection "" []

title' :: String -> Html
title' = title . toHtml
