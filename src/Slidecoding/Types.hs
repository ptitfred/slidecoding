{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Slidecoding.Types
    ( Context(..)
    , ContextName
    , Description(..)
    , Design(..)
    , Item(..)
    , Metadata(..)
    , Module(..)
    , ModuleName
    , Name
    , Pixel
    , Port
    , Presentation(..)
    , Signature(..)
    , Source(..)
    , Stream(..)
    , Symbol(..)
    , TransitionStyle(..)
    , Theme(..)
    , ValidationMessage
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Foldable    (asum)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type Port = Int
type Name = String
type ModuleName = String

data Module       = Module FilePath ModuleName
newtype Symbol    = Symbol String
newtype Signature = Signature String

data Description = Description Module [Item]
data Item = Item Symbol Signature Source
newtype Source = Source String

data Stream m where
  Stream :: Monad m => m ()             -- prepare
                    -> m String         -- readInput
                    -> (String -> m ()) -- writeOutput
                    -> Stream m

type ContextName = String

data Context = Context { name         ::  ContextName
                       , mainModule   ::  ModuleName
                       , otherModules :: [ModuleName]
                       }

data Presentation = Presentation { rootDir  :: FilePath
                                 , distDir  :: FilePath
                                 , metadata :: Metadata
                                 }

data Metadata = Metadata { title    ::  String
                         , design   ::  Design
                         , contexts :: [Context]
                         }

data Design = Design { width      :: Pixel
                     , height     :: Pixel
                     , theme      :: Maybe Theme
                     , icon       :: Maybe FilePath
                     , transition :: Maybe TransitionStyle
                     }

type Pixel = Int
data Theme = Builtin String | Custom FilePath | Patch Theme FilePath
data TransitionStyle = HorizontalSlide | VerticalSlide | Fade

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .:  "title"
                                  <*> v .:  "design"
                                  <*> v .:? "contexts" .!= []
  parseJSON invalid    = typeMismatch "Metadata" invalid

instance FromJSON Design where
  parseJSON (Object v) = Design <$> v .:? "width"  .!= 1600
                                <*> v .:? "height" .!= 1000
                                <*> v .:? "theme"
                                <*> v .:? "icon"
                                <*> v .:? "transition"
  parseJSON invalid    = typeMismatch "Design" invalid

data PartialContext = PartialContext ModuleName [ModuleName]

instance FromJSON PartialContext where
  parseJSON (Object o) = PartialContext <$> o .:  "module"
                                        <*> o .:? "others" .!= []
  parseJSON invalid    = typeMismatch "PartialContext" invalid

instance FromJSON [Context] where
  parseJSON (Object v) = for (HM.toList v) $ \ (n, ctx) -> buildContext (T.unpack n) <$> parseJSON ctx
    where buildContext n (PartialContext m mn) = Context n m mn
  parseJSON invalid    = typeMismatch "Context" invalid

instance FromJSON Theme where
  parseJSON (Object v) =
    asum [
      Patch  <$> getBase <*> getCustom,
      Custom <$> getCustom,
      getBase
    ]
      where getBase   = Builtin <$> v .: "base"
            getCustom = v .: "custom"
  parseJSON invalid    = typeMismatch "Theme" invalid

instance FromJSON TransitionStyle where
  parseJSON (String "fade")             = return Fade
  parseJSON (String "horizontal-slide") = return HorizontalSlide
  parseJSON (String "vertical-slide")   = return VerticalSlide
  parseJSON  invalid                    = typeMismatch "TransitionStyle" invalid

type ValidationMessage = String
