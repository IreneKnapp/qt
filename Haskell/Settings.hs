{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleContexts #-}
module Settings
  (Settings(..),
   OwnerSettings(..),
   DatabaseSettings(..),
   ProcessSettings(..),
   NetworkSettings(..),
   SelfSettings(..),
   ChannelSettings(..),
   NickservSettings(..),
   NickservCommandSettings(..),
   ServiceCommandSettings(..),
   ServerSettings(..))
  where

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

import Control.Applicative
import Control.Monad
import Data.Yaml

import Types


class FromJSON' data' parameter where
  parseJSON' :: parameter -> Value -> Parser data'


showJSON :: Value -> String
showJSON = Text.unpack . Text.decodeUtf8 . encode


parseList :: (FromJSON data') => Value -> Parser [data']
parseList (Array vector) = do
  Vector.foldM' (\soFar value -> do
                   result <- parseJSON value
                   return $ soFar ++ [result])
                []
                vector
parseList value =
  fail $ "Expected an array, but got " ++ (showJSON value) ++ "."


parseSet :: (Ord data', FromJSON data') => Value -> Parser (Set.Set data')
parseSet (Array vector) = do
  Vector.foldM' (\soFar value -> do
                   result <- parseJSON value
                   return $ Set.insert result soFar)
                Set.empty
                vector
parseSet value =
  fail $ "Expected an array, but got " ++ (showJSON value) ++ "."


parseMap
  :: (FromJSON' data' Text.Text)
  => Value -> Parser (Map.Map Text.Text data')
parseMap (Object hashMap) = do
  foldM (\soFar (name, value) -> do
           result <- parseJSON' name value
           return $ Map.insert name result soFar)
        Map.empty
        (HashMap.toList hashMap)
parseMap value =
  fail $ "Expected an object, but got " ++ (showJSON value) ++ "."


data Settings = Settings {
    settingsOwner :: OwnerSettings,
    settingsDatabase :: DatabaseSettings,
    settingsProcess :: ProcessSettings,
    settingsNetworks :: Map.Map Text.Text NetworkSettings
  }
  deriving (Show)
instance FromJSON Settings where
  parseJSON (Object value) = do
    Settings
      <$> value .: "owner"
      <*> value .: "database"
      <*> value .: "process"
      <*> (value .: "networks" >>= parseMap)
  parseJSON _ = fail "Expected settings object."


data OwnerSettings = OwnerSettings {
    ownerSettingsRealName :: Text.Text,
    ownerSettingsEmail :: Text.Text
  }
  deriving (Show)
instance FromJSON OwnerSettings where
  parseJSON (Object value) = do
    OwnerSettings
      <$> value .: "real-name"
      <*> value .: "email"
  parseJSON _ = fail "Expected owner-settings object."


data DatabaseSettings = DatabaseSettings {
    databaseSettingsPath :: Text.Text
  }
  deriving (Show)
instance FromJSON DatabaseSettings where
  parseJSON (String value) = do
    DatabaseSettings
      <$> return value
  parseJSON _ = fail "Expected database-settings string."


data ProcessSettings = ProcessSettings {
    processSettingsPidfile :: Text.Text,
    processSettingsErrorLog :: Text.Text,
    processSettingsUser :: Text.Text,
    processSettingsGroup :: Text.Text,
    processSettingsDaemonize :: Bool
  }
  deriving (Show)
instance FromJSON ProcessSettings where
  parseJSON (Object value) = do
    ProcessSettings
      <$> value .: "pidfile"
      <*> value .: "error-log"
      <*> value .: "user"
      <*> value .: "group"
      <*> value .: "daemonize"
  parseJSON _ = fail "Expected process-settings object."


data NetworkSettings = NetworkSettings {
    networkSettingsName :: Text.Text,
    networkSettingsSelf :: SelfSettings,
    networkSettingsChannels :: Map.Map Text.Text ChannelSettings,
    networkSettingsNickserv :: NickservSettings,
    networkSettingsServers :: [ServerSettings]
  }
  deriving (Show)
instance FromJSON' NetworkSettings Text.Text where
  parseJSON' name (Object value) = do
    NetworkSettings
      <$> return name
      <*> value .: "self"
      <*> (value .: "channels" >>= parseMap)
      <*> value .: "nickserv"
      <*> (value .: "servers" >>= parseList)
  parseJSON' _ _ = fail "Expected network-settings object."


data SelfSettings = SelfSettings {
    selfSettingsNickname :: Text.Text,
    selfSettingsNickservPassword :: Text.Text,
    selfSettingsMode :: Text.Text
  }
  deriving (Show)
instance FromJSON SelfSettings where
  parseJSON (Object value) = do
    SelfSettings
      <$> value .: "nickname"
      <*> value .: "nickserv-password"
      <*> value .: "mode"


data ChannelSettings = ChannelSettings {
    channelSettingsName :: Text.Text
  }
  deriving (Show)
instance FromJSON' ChannelSettings Text.Text where
  parseJSON' name (Object value) = do
    ChannelSettings
      <$> return name
  parseJSON' _ _ = fail "Expected channel-settings object."


data NickservSettings = NickservSettings {
    nickservSettingsNickname :: Text.Text,
    nickservSettingsCommands :: NickservCommandSettings
  }
  deriving (Show)
instance FromJSON NickservSettings where
  parseJSON (Object value) = do
    NickservSettings
      <$> value .: "nickname"
      <*> value .: "commands"
  parseJSON _ = fail "Expected nickserv-settings object."


data NickservCommandSettings = NickservCommandSettings {
    nickservCommandSettingsRegister :: ServiceCommandSettings,
    nickservCommandSettingsIdentify :: ServiceCommandSettings
  }
  deriving (Show)
instance FromJSON NickservCommandSettings where
  parseJSON (Object value) = do
    NickservCommandSettings
      <$> value .: "register"
      <*> value .: "identify"
  parseJSON _ = fail "Expected nickserv-command-settings object."


data ServiceCommandSettings = ServiceCommandSettings {
    serviceCommandSettingsTemplate :: [TemplatePart]
  }
  deriving (Show)
instance FromJSON ServiceCommandSettings where
  parseJSON (String value) = do
    ServiceCommandSettings
      <$> (case parseTemplate value of
             Just parts -> return parts
             _ -> fail "Template doesn't parse.")


data ServerSettings = ServerSettings {
    serverSettingsHost :: Text.Text,
    serverSettingsPorts :: Set.Set Int
  }
  deriving (Show)
instance FromJSON ServerSettings where
  parseJSON (Object value) = do
    ServerSettings
      <$> value .: "host"
      <*> (value .: "ports" >>= parseSet)
