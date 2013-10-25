{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Import

import Configuration


main :: IO ()
main = mainWrapper $ \settings -> do
  $(logDebug) "Hmm"
