module Main (main) where

import Configuration
import Settings
import Types


main :: IO ()
main = mainWrapper $ \settings -> do
  putStrLn $ show settings
