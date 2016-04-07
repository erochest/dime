module Dime.Utils where


import           Control.Error


putStrLn' :: String -> Script ()
putStrLn' = scriptIO . putStrLn
