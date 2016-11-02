{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Assets
    ( (<>)
    , Asset(..)
    , distribute
    , include
    ) where

import Slidecoding.Types                  (Presentation(..))

import Paths_slidecoding                  (getDataFileName)

import Data.Monoid                        ((<>))
import Data.String                        (IsString(..), fromString)
import System.Directory                   (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath                    ((</>), takeDirectory)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

data Asset = CSS String | CSSPrint String | JS String | InlineJS String | Favicon String | Bundle [Asset]

distribute :: Presentation -> Asset -> IO ()
distribute presentation (Bundle assets) = mapM_ (distribute presentation) assets
distribute            _ (InlineJS    _) = return ()
distribute presentation (CSS      path) = distribute' presentation path
distribute presentation (CSSPrint path) = distribute' presentation path
distribute presentation (JS       path) = distribute' presentation path
distribute presentation (Favicon  path) = distribute' presentation path

include :: Asset -> Html
include (CSS         path) = css "screen" (fromString path)
include (CSSPrint    path) = css "print"  (fromString path)
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

distribute' :: Presentation -> FilePath -> IO ()
distribute' presentation filename = getOriginal >>= copy
  where getOriginal = findOriginal (rootDir presentation) filename
        copy from   = safeCopyFile from to
        to          = distDir presentation </> filename

findOriginal :: FilePath -> FilePath -> IO FilePath
findOriginal directory filename = do
  let localFile = directory </> filename
  localExists <- doesFileExist localFile
  if localExists
  then return localFile
  else getDataFileName filename

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = do
  createDirectoryIfMissing True (takeDirectory to)
  copyFile from to

favicon :: AttributeValue -> Html
favicon file = link ! rel "icon"
                    ! type_ "image/png"
                    ! href file

css :: AttributeValue -> AttributeValue -> Html
css m file = link ! rel "stylesheet"
                  ! media m
                  ! href file

script' :: AttributeValue -> Html
script' file = script ! src file
                      $ toHtml empty

javascript :: String -> Html
javascript = script . toHtml

empty :: String
empty = ""
