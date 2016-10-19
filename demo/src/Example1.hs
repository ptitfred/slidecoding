module Example1
    ( parseCSV
    , parseJSON
    , load
    ) where

type CSV = String
type JSON = String

parseCSV :: FilePath -> IO CSV
parseCSV _ = return ""

parseJSON :: FilePath -> IO JSON
parseJSON _ = return ""

load :: FilePath -> IO String
load = readFile
