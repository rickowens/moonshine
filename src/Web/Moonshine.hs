{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns #-}
module Web.Moonshine (
  Moonshine,
  LoggingConfig(..),
  runMoonshine,
  route
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), (.:?))
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Yaml (FromJSON(parseJSON), decodeFileEither)
import GHC.Generics (Generic)
import Snap (Snap, quickHttpServe)
import System.Directory (createDirectoryIfMissing)
import System.Log (Priority)
import System.Metrics.Distribution (Distribution)
import System.Remote.Monitoring (Server, forkServer, getDistribution)
import qualified Data.Text as T
import qualified Snap (route)
import qualified System.Metrics.Distribution as D (add)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------

{- |
  This type is the type of things that can be run by the moonshine framework.
  Generate values of this type using `route`, and run the value as a web
  service using `runMoonshine`.
-}
data Moonshine = M [(ByteString, Snap ())]


{- |
  Logging configuration that Moonshine should use to initialize logging.

  This type is an instance of FromJSON, so you can easily use it in your
  configuration as:

  @
  data MyConfig = MyConfig {
    logging :: LoggingConfig
  } deriving (Generic)
  instance FromJSON MyConfig
  @
-}
data LoggingConfig =
  LoggingConfig {
    level :: LogPriority
  } deriving (Generic)

instance FromJSON LoggingConfig


{- |
  A wrapper for Priority, so we can avoid orphan instances
-}
newtype LogPriority = LP Priority

instance FromJSON LogPriority where
  parseJSON (String s) = case reads (T.unpack s) of
    [(priority, "")] -> return (LP priority)
    _ -> fail $ "couldn't parse Priority from string " ++ show s
  parseJSON value = fail $ "Couldn't parse Priority from value " ++ show value


-- Public Functions -----------------------------------------------------------

{- |
  Run a `Moonshine` value that was generated from a user-defined configuration.
-}
runMoonshine :: (FromJSON a) => (a -> Moonshine) -> IO ()
runMoonshine initialize = do
  (userConfig, systemConfig) <- loadConfig configPath
  setupLogging systemConfig
  metricsServer <- forkServer "0.0.0.0" 8001
  let M routes = initialize userConfig
  (quickHttpServe . Snap.route) =<< mapM (monitorRoute metricsServer) routes

  where
    monitorRoute :: Server -> (ByteString, Snap ()) -> IO (ByteString, Snap ())
    monitorRoute server (path, snap) = do -- IO monad
      timer <- getDistribution (decodeUtf8 path) server
      return (path, monitoredRoute timer snap)
      
    monitoredRoute :: Distribution -> Snap () -> Snap ()
    monitoredRoute timer snap = do -- snap monad
      start <- liftIO getCurrentTime
      result <- snap
      end <- liftIO getCurrentTime
      addTiming start end
      return result
      where
        addTiming start end = liftIO $
          D.add timer diff
          where
            diff = toDouble (diffUTCTime end start)
            toDouble = fromRational . toRational


{- |
  Like `Snap.route`, but that automatically sets up metrics for the
  specified routes.
-}
route :: [(ByteString, Snap ())] -> Moonshine
route = M


-- Private Types --------------------------------------------------------------

{- |
  Defines all the "system" config, where "system" means everything that
  Moonshine knows about.
-}
data SystemConfig =
  SystemConfig {
    logging :: Maybe LoggingConfig
  }

instance FromJSON SystemConfig where
  parseJSON (Object topLevel) = do
    logging <- topLevel .:? "logging"
    return SystemConfig {logging}
  parseJSON value =
    fail $ "Couldn't parse system config from value " ++ show value


-- Private Functions ----------------------------------------------------------

{- |
  Do all of the things that it takes to get logging set up the way we
  want it.
-}
setupLogging :: SystemConfig -> IO ()
setupLogging SystemConfig {logging = Nothing} =
  createDirectoryIfMissing True "log"
setupLogging SystemConfig {logging = Just _loggingConfig} =
  -- FIXME
  putStrLn "FIXME: setting up logging somehow"


{- |
  hard coded config file path.
-}
configPath :: FilePath
configPath = "config.yml"


{- |
  Load the configuration from YAML.
-}
loadConfig :: FromJSON a => FilePath -> IO (a, SystemConfig)
loadConfig path = do
  eConfig <- decodeFileEither path
  case eConfig of
    Left errorMsg -> error $
      "Couldn't decode YAML config from file "
      ++ path ++ ": " ++ show errorMsg
    Right config -> return config


