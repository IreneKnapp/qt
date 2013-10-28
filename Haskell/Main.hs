{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Import

import qualified Control.Concurrent.MSem as MSem
import qualified Data.Map as Map
import qualified Data.Text as Text

import IRC


main :: IO ()
main = mainWrapper $ \settings -> do
  semaphore <- liftBase $ MSem.new 0
  mapM_ (\(_, networkSettings) -> do
           _ <- fork $ runConnectionT $ catch
             (sourceIRC networkSettings $$ awaitForever $ \event -> do
                case event of
                  ConnectEvent -> do
                    let selfSettings = networkSettingsSelf networkSettings
                        nickname = selfSettingsNickname selfSettings
                    send $ Message Nothing "NICK" [nickname]
                  DisconnectEvent -> return ()
                  MessageEvent message -> do
                   $(logInfo) (formatMessage message))
             (\e -> do
                return (e :: SomeException)
                liftBase $ MSem.signal semaphore)
           return ())
        (Map.toList $ settingsNetworks settings)
  mapM_ (\_ -> liftBase $ MSem.wait semaphore)
        (Map.toList $ settingsNetworks settings)
