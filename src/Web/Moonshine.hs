module Web.Moonshine (
  runMoonshine,
  route
) where

import Data.ByteString (ByteString)
import Snap (Snap, quickHttpServe, MonadSnap)
import Data.Yaml (FromJSON, decodeFileEither)
import qualified Snap (route)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------
-- Public Functions -----------------------------------------------------------

{- |
  Run a snap web service in the moonshine framework.
-}
runMoonshine :: FromJSON a => (a -> Snap ()) -> IO ()
runMoonshine init = do
  let configPath = "config.yml"
  config <- loadConfig configPath
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

-- Private Types --------------------------------------------------------------
-- Private Functions ----------------------------------------------------------


