{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Web.Moonshine (
  Moonshine,
  LoggingConfig,
  HasLoggingConfig(..),
  runMoonshine,
  route
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Yaml (FromJSON(parseJSON), decodeFileEither)
import GHC.Generics (Generic)
import Snap (Snap, quickHttpServe, MonadSnap)
import System.Directory (createDirectoryIfMissing)
import System.Log (Priority)
import System.Metrics.Distribution (Distribution)
import System.Remote.Monitoring (Server, forkServer, getDistribution)
import qualified Data.Text as T
import qualified Snap (route)
import qualified System.Metrics.Distribution as D (add)

-- Public Types ---------------------------------------------------------------

{- |
  Your Config type must be an instance of HasLoggingConfig.
-}
class HasLoggingConfig a where
  {- |
    Extract the LoggingConfig from your config type.

    This can be Nothing if your application allows an empty or absent
    logging config; in this case, Moonshine will set up some sensible
    defaults.

    See sunrise for an example of an application that does NOT support
    an absent logging config.
  -}
  getLoggingConfig :: a -> Maybe LoggingConfig
  getLoggingConfig _ = Nothing


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
    level :: Priority
  } deriving (Generic)

instance FromJSON LoggingConfig
instance FromJSON Priority where
  parseJSON (String s) = case reads (T.unpack s) of
    [(priority, "")] -> return priority
    _ -> fail $ "couldn't parse Priority from string " ++ show s
  parseJSON value = fail $ "Couldn't parse Priority from value " ++ show value


-- Public Functions -----------------------------------------------------------

{- |
  Run a `Moonshine` value that was generated from a user-defined configuration.
-}
runMoonshine :: (FromJSON a, HasLoggingConfig a) => (a -> Moonshine) -> IO ()
runMoonshine init = do
  config <- loadConfig configPath
  setupLogging config
  metricsServer <- forkServer "0.0.0.0" 8001
  let M routes = init config
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
      addTiming timer start end
      return result
      where
        addTiming timer start end = liftIO $
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
-- Private Functions ----------------------------------------------------------

{- |
  Conditionally execute an action depending on the value of the Maybe.
  This is like the "when" from Control.Monad, combined with pattern matching.
-}
whenMaybe :: Maybe a -> (a -> IO ()) -> IO ()
whenMaybe Nothing _= return ()
whenMaybe (Just a) f = f a


{- |
  Do all of the things that it takes to get logging set up the way we
  want it.
-}
setupLogging config = do
  createDirectoryIfMissing True "log"
  whenMaybe (getLoggingConfig config) configureLogging
  where
    {- |
      FIXME
    -}
    configureLogging :: LoggingConfig -> IO ()
    configureLogging loggingConfig =
      putStrLn "FIXME: setting up logging somehow"


{- |
  hard coded config file path.
-}
configPath = "config.yml"


{- |
  Load the configuration from YAML.
-}
loadConfig :: FromJSON a => FilePath -> IO a
loadConfig path = do
  eConfig <- decodeFileEither path
  case eConfig of
    Left errorMsg -> error $
      "Couldn't decode YAML config from file "
      ++ path ++ ": " ++ show errorMsg
    Right config -> return config


