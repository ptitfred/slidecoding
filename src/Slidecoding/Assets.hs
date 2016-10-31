{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Assets
    ( (<>)
    , Asset(..)
    , distribute
    , include
    ) where

import Paths_slidecoding                  (getDataFileName)

import Data.Monoid                        ((<>))
import Data.String                        (IsString(..), fromString)
import System.Directory                   (copyFile, createDirectoryIfMissing)
import System.FilePath                    ((</>), takeDirectory)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

data Asset = CSS String | JS String | InlineJS String | Favicon String | Bundle [Asset]

distribute :: FilePath -> Asset -> IO ()
distribute directory (Bundle assets) = mapM_ (distribute directory) assets
distribute directory (CSS      path) = distribute' directory path
distribute directory (JS       path) = distribute' directory path
distribute         _ (InlineJS    _) = return ()
distribute directory (Favicon  path) = distribute' directory path

include :: Asset -> Html
include (CSS         path) = css     (fromString path)
include (JS          path) = script' (fromString path)
include (InlineJS source') = javascript source'
include (Favicon     path) = favicon (fromString path)
include (Bundle    assets) = mapM_ include assets

instance Monoid Asset where
  mempty = Bundle []
  mappend (Bundle b1) (Bundle b2) = Bundle (b1 ++ b2)
  mappend (Bundle b1)         a2  = Bundle (b1 ++ [a2])
  mappend         a1  (Bundle b2) = Bundle (a1 : b2)
  mappend         a1          a2  = Bundle [a1, a2]

distribute' :: FilePath -> FilePath -> IO ()
distribute' directory filename = getOriginal >>= copy
  where getOriginal = getDataFileName filename
        copy from   = safeCopyFile from to
        to          = directory </> filename

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = do
  createDirectoryIfMissing True (takeDirectory to)
  copyFile from to

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

empty :: String
empty = ""
