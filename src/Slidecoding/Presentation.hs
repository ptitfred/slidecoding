{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Presentation
    ( Presentation(..)
    , Metadata(..)
    , ValidationMessage
    , load
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Either      (isRight)
import Data.Text        (Text)
import Data.Yaml        (decodeFileEither, ParseException)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath  ((</>))

data Presentation = Presentation { rootDir :: FilePath
                                 , meta    :: Metadata
                                 } deriving Show

data Metadata = Metadata { title :: Text
                         } deriving Show

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .: "title"
  parseJSON (Array _)  = return $ Metadata "array"
  parseJSON invalid    = typeMismatch "Metadata" invalid

type ValidationMessage = String
type Validation = Maybe ValidationMessage
type Validator = FilePath -> IO Validation

load :: FilePath -> IO (Either ValidationMessage Presentation)
load dir = checkAll (expectations dir) >>= hopefullyReturn loadPresentation
  where loadPresentation = fmap (Presentation dir) <$> loadYaml (dir </> "presentation.yaml")

expectations :: FilePath -> [IO Validation]
expectations dir = [ expectDirectory dir
                   , expectFile descriptor
                   , expectYaml descriptor
                   ]
  where descriptor = dir </> "presentation.yaml"

expectDirectory, expectFile, expectYaml :: Validator

expectDirectory path = expect msg (doesDirectoryExist path)
  where msg = path ++ " is not a directory"

expectFile path = expect msg (doesFileExist path)
  where msg = path ++ " is missing or is not a file"

expectYaml path = expect msg validYaml
  where msg = path ++ " is not a valid Yaml file"
        validYaml = isRight <$> loadMetadata path

loadYaml :: FilePath -> IO (Either ValidationMessage Metadata)
loadYaml p = either handleError Right <$> loadMetadata p
  where handleError e = Left ("Invalid Metadata: " ++ show e :: ValidationMessage)

loadMetadata :: FilePath -> IO (Either ParseException Metadata)
loadMetadata = decodeFileEither

hopefullyReturn :: IO (Either ValidationMessage Presentation) -> Validation -> IO (Either ValidationMessage Presentation)
hopefullyReturn action = maybe action (return . Left)

checkAll :: [IO Validation] -> IO Validation
checkAll [] = return Nothing
checkAll (io : ios) = io >>= check
  where check Nothing  = checkAll ios
        check (Just m) = return (Just m)

expect :: String -> IO Bool -> IO Validation
expect msg p = say <$> p
  where say True  = Nothing
        say False = Just msg
