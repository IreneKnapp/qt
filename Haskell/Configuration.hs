module Configuration
  (DeploymentEnvironment(..),
   mainWrapper)
  where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as YAML

import Control.Monad.Logger
import System.Environment
import System.Exit

import Settings
import Types


data DeploymentEnvironment
  = Development
  | Production
  deriving (Eq, Ord, Read, Show)


mainWrapper :: (Settings -> Main ()) -> IO ()
mainWrapper innerMain = do
  let expectFilePath :: [String] -> Maybe (FilePath, [String])
      expectFilePath (filePath : rest) = Just (filePath, rest)
      expectFilePath _ = Nothing
      expectDeploymentEnvironment
        :: [String] -> Maybe (DeploymentEnvironment, [String])
      expectDeploymentEnvironment (environmentName : rest) =
        case reads environmentName of
          [(environment, "")] -> Just (environment, rest)
          _ -> Nothing
      expectDeploymentEnvironment _ = Nothing
      expectEnd :: [String] -> Maybe ()
      expectEnd [] = Just ()
      expectEnd _ = Nothing
      parseArguments :: [String] -> Maybe (FilePath, DeploymentEnvironment)
      parseArguments arguments = do
        (configFilePath, arguments) <- expectFilePath arguments
        (deploymentEnvironment, arguments) <-
          expectDeploymentEnvironment arguments
        expectEnd arguments
        return (configFilePath, deploymentEnvironment)
  arguments <- getArgs
  case parseArguments arguments of
    Just (configFilePath, deploymentEnvironment) -> do
      eitherSettingsMap <- YAML.decodeFileEither configFilePath
      case eitherSettingsMap of
        Left exception -> do
          putStrLn $ show exception
          exitFailure
        Right settingsMap -> do
          let key = Text.pack $ show deploymentEnvironment
          case Map.lookup key settingsMap of
            Just settings -> do
              if processSettingsDaemonize $ settingsProcess settings
                then do
                  putStrLn "Not implemented."
                  exitFailure
                else do
                  runStdoutLoggingT $ do
                    innerMain settings
            Nothing -> do
              putStrLn $
                "Configuration file contains no information for "
                ++ (Text.unpack key) ++ "."
              exitFailure
    Nothing -> do
      putStrLn $ "Usage: qt config.yaml (Development|Production)"
      exitFailure
