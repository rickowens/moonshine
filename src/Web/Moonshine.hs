{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns #-}
module Web.Moonshine (
  Moonshine,
  LoggingConfig(..),
  runMoonshine,
  route,
  makeTimer,
  timerAdd,
  Timer,
  getUserConfig,
  timed
) where

import Control.Applicative (liftA2, Applicative(pure, (<*>)))
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (Value(..), (.:?))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Yaml (FromJSON(parseJSON), decodeFileEither)
import GHC.Generics (Generic)
import Snap (Snap, httpServe, setPort, Config, setSSLCert, setSSLKey, setSSLPort)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Log (Priority(..))
import System.Metrics.Distribution (Distribution)
import System.Remote.Monitoring (Server, forkServerWith)
import qualified Data.Text as T
import qualified System.Metrics as EkgMetrics
import qualified Snap (route)
import qualified System.Metrics.Distribution as D (add)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------

{- |
  The moonshine monad.
-}
data Moonshine config a = Moonshine (State config -> IO (State config, a))

instance Monad (Moonshine config) where
  return a = Moonshine (\state -> return (state, a))
  (Moonshine m) >>= fun = Moonshine $ \state -> do
    (newState, val) <- m state
    let Moonshine m2 = fun val
    m2 newState

instance Applicative (Moonshine config) where
  pure = return
  (<*>) = ap

instance Functor (Moonshine config) where
  fmap = liftM

instance MonadIO (Moonshine config) where
  liftIO io = Moonshine $ \state -> do
    val <- io
    return (state, val)


{- |
  The Timer type.
-}
newtype Timer = T Distribution


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


data ServerConfig =
  ServerConfig {
    applicationConnector :: [ConnectorConfig]
    -- | Configuration for EKG.  Note that although this is a list, at present EKG makes it impossible to support more
    -- than one server. See https://github.com/tibbe/ekg/issues/32.
  , adminConnector :: [ConnectorConfig]
  } deriving (Generic)

instance FromJSON ServerConfig


data ConnectorConfig = ConnectorConfig {
    scheme :: Scheme
  , port :: Int
  , cert :: Maybe FilePath
  , key :: Maybe FilePath
  } deriving (Generic)

instance FromJSON ConnectorConfig


data Scheme = HTTP | HTTPS
  deriving (Generic)

instance FromJSON Scheme


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
  Execute an insance of the Moonshine monad. This starts up a server
  and never returns.
-}
runMoonshine :: (FromJSON config) => Moonshine config a -> IO a
runMoonshine (Moonshine m) = do
  printBanner
  (userConfig, systemConfig) <- loadConfig configPath
  setupLogging systemConfig
  metricsStore <- EkgMetrics.newStore
  (State {snap}, val) <- m State {userConfig, snap = return (), metricsStore}
  startServer (serverConfig systemConfig) metricsStore snap
  return val
  where
    serverConfig SystemConfig { server = mServerConfig } = fromMaybe defaultServerConfig mServerConfig


{- |
  Returns the user config.
-}
getUserConfig :: Moonshine config config
getUserConfig = Moonshine (\state@State {userConfig} ->
    return (state, userConfig)
  )


{- |
  Like `Snap.route`, but that automatically sets up metrics for the
  specified routes.
-}
route :: [(ByteString, Snap ())] -> Moonshine config ()
route routes = Moonshine (\state@State {snap, metricsStore} -> do
    monitoredRoutes <- mapM (monitorRoute metricsStore) routes
    return (state {snap = snap >> Snap.route monitoredRoutes}, ())
  )
  where
    monitorRoute :: EkgMetrics.Store -> (ByteString, Snap ()) -> IO (ByteString, Snap ())
    monitorRoute metricsStore (path, snap) = do -- IO monad
      timer <- getDistribution (decodeUtf8 path) metricsStore
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
  Wrap a `Moonshine` in a timer, so that timing data for invocations
  will be logged to the metrics handler.
-}
timed
  :: T.Text
    -- ^ The name of the timer.
  -> Moonshine config a
    -- ^ The action to be wrapped.
  -> Moonshine config a
timed name (Moonshine m) = Moonshine $ \state@State {snap, metricsStore} -> do
  timer <- getDistribution name metricsStore
  m state {snap = timedSnap timer snap}
  where
    timedSnap :: Distribution -> Snap a -> Snap a
    timedSnap timer snap = do -- snap monad
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
  Make a new timer with the given name.
-}
makeTimer :: T.Text -> Moonshine config Timer
makeTimer name = Moonshine (\state@State {metricsStore} -> do
    dist <- getDistribution name metricsStore
    return (state, T dist)
  )


{- |
  Add a time to a timer.
-}
-- timerAdd :: (Real time) => Timer -> time -> Moonshine config ()
timerAdd :: (Real time) => Timer -> time -> IO ()
timerAdd (T timer) = D.add timer . fromRational . toRational


-- Private Types --------------------------------------------------------------

{- |
  Defines all the "system" config, where "system" means everything that
  Moonshine knows about.
-}
data SystemConfig =
  SystemConfig {
    logging :: Maybe LoggingConfig
  , server :: Maybe ServerConfig
  }

instance FromJSON SystemConfig where
  parseJSON (Object topLevel) = do
    logging <- topLevel .:? "logging"
    server <- topLevel .:? "server"
    return SystemConfig {logging, server}
  parseJSON value =
    fail $ "Couldn't parse system config from value " ++ show value


{- |
  The internal running state used while executing the `Moonshine` monad.
-}
data State config =
  State {
    userConfig :: config,
    snap :: Snap (),
    metricsStore :: EkgMetrics.Store
  }


-- Private Functions ----------------------------------------------------------

setupLogging :: SystemConfig -> IO ()
setupLogging SystemConfig {logging} = installLoggingConfig (fromMaybe defaultLoggingConfig logging)

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig {
  level = LP INFO
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig {
    adminConnector = [ConnectorConfig {scheme=HTTP, port=8001, cert=Nothing, key=Nothing}]
  , applicationConnector = [ConnectorConfig {scheme=HTTP, port=8000, cert=Nothing, key=Nothing}]
  }

{- |
  Do all of the things that it takes to get logging set up the way we
  want it.
-}
installLoggingConfig :: LoggingConfig -> IO ()
installLoggingConfig _loggingConfig = do
  -- FIXME
  createDirectoryIfMissing True "log"
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
  eUserConfig <- decodeFileEither path
  eSystemConfig <- decodeFileEither path
  case liftA2 (,) eUserConfig eSystemConfig of
    Left errorMsg -> error $
      "Couldn't decode YAML config from file "
      ++ path ++ ": " ++ show errorMsg
    Right configs -> return configs


{- |
  Find and output a banner.
-}
printBanner :: IO ()
printBanner = do
  exists <- doesFileExist filepath
  banner <- if exists
            then readFile filepath
            else return $ "FIXME: missing " ++ filepath ++ ".  son, I am disappoint\n"
  putStr banner
  where
    filepath = "lib/banner.txt"


{- |
  A replacement for 'System.Remote.Monitoring.getDistribution' that uses an 'EkgMetrics.Store' instead of a 'Server'.
-}
getDistribution :: T.Text -> EkgMetrics.Store -> IO Distribution
getDistribution = EkgMetrics.createDistribution

{- |
  Start application listening on the ports given by the config.
-}
startServer :: ServerConfig -> EkgMetrics.Store -> Snap () -> IO ()
startServer ServerConfig { applicationConnector, adminConnector } metricsStore snap = do
  mapM_ startMetricsServer adminConnector
  httpServe snapConfig snap

  where
    startMetricsServer :: ConnectorConfig -> IO Server
    startMetricsServer ConnectorConfig { scheme=HTTP, port } = forkServerWith metricsStore "0.0.0.0" port
    startMetricsServer ConnectorConfig { scheme=HTTPS } = error "EKG does not support running on HTTPS"

    snapConfig :: Config m a
    snapConfig = mconcat $ map toSnapConfig applicationConnector
    toSnapConfig :: ConnectorConfig -> Config m a
    toSnapConfig ConnectorConfig { scheme=HTTP, port } = setPort port mempty
    toSnapConfig ConnectorConfig { scheme=HTTPS, port, cert=mcert, key=mkey } =
      fromMaybe
        (error "You must provide cert and key in order to use HTTPS.")
        $ do -- Maybe monad
          cert <- mcert
          key <- mkey
          return $ setSSLCert cert $ setSSLKey key $ setSSLPort port mempty
