module Utils where

inspect :: Show a => IO a -> IO ()
inspect = (=<<) print
