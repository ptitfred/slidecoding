module Example2 where

data SweetData = SweetData Int Char deriving Show
data Wrapper = Wrapper SweetData deriving Show

class Parseable a where
  parse :: String -> a

instance Parseable SweetData where
  parse text = SweetData (read i) c
    where [i, [c]] = words text

instance Parseable Wrapper where
  parse text = Wrapper $ parse text
