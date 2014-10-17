{-# LANGUAGE DeriveGeneric #-}
module Web.Moonshine (
  LoggingConfig(), HasLoggingConfig(..),
  runMoonshine,
  route
) where

import GHC.Generics (Generic)

import qualified Data.Text as T
import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)
import Snap (Snap, quickHttpServe, MonadSnap)
import Data.Yaml (FromJSON(..), decodeFileEither)
import Data.Aeson (Value(..))
import System.Log (Priority())
import qualified Snap (route)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------
-- Public Functions -----------------------------------------------------------

{- |
  Run a snap web service in the moonshine framework.
-}
runMoonshine :: (FromJSON a, HasLoggingConfig a) => (a -> Snap ()) -> IO ()
runMoonshine init = do
  createDirectoryIfMissing True "log"
  let configPath = "config.yml"
  config <- loadConfig configPath
  whenMaybe (getLoggingConfig config) setupLogging
  quickHttpServe (init config)


{- |
  Load the configuration from YAML.
-}
loadConfig :: FromJSON a => FilePath -> IO a
loadConfig path = do
  eConfig <- decodeFileEither path
  case eConfig of
    Left errorMsg -> error $ "Couldn't decode YAML config from file " ++ path ++ ": " ++ (show errorMsg)
    Right config -> return config

{- |
  A version of `Snap.route` that automatically sets up metrics for the specified routes.
-}
route :: MonadSnap m => [(ByteString, m a)] -> m a
route = Snap.route

data LoggingConfig = LoggingConfig {
  level :: Priority
} deriving (Generic)

class HasLoggingConfig a where
  getLoggingConfig :: a -> Maybe LoggingConfig
  getLoggingConfig _ = Nothing

instance FromJSON LoggingConfig
instance FromJSON Priority where
  parseJSON (String s) = case reads (T.unpack s) of
    [(priority, "")] -> return priority
    _ -> fail $ "couldn't parse Priority from string " ++ show s
  parseJSON value = fail $ "Couldn't parse Priority from value " ++ show value

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
  FIXME
-}
setupLogging :: LoggingConfig -> IO ()
setupLogging loggingConfig = putStrLn "FIXME: setting up logging somehow"
