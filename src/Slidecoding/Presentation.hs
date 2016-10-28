{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Presentation
    ( load
    ) where

import Slidecoding.Types

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Either      (isRight)
import Data.Yaml        (decodeFileEither, ParseException)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath  ((</>))

newtype MetadataWrapper = MetadataWrapper Metadata

unwrapMetadata :: MetadataWrapper -> Metadata
unwrapMetadata (MetadataWrapper m) = m

instance FromJSON MetadataWrapper where
  parseJSON (Object v) = MetadataWrapper . Metadata <$> v .: "title"
  parseJSON (Array _)  = return $ MetadataWrapper (Metadata "array")
  parseJSON invalid    = typeMismatch "Metadata" invalid

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
loadMetadata file = fmap unwrapMetadata <$> decodeFileEither file

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
