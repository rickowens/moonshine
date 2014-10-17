module Web.Moonshine (
  runMoonshine
) where

import Data.ByteString (ByteString)
import Snap (Snap, quickHttpServe, MonadSnap)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------
-- Public Functions -----------------------------------------------------------

{- |
  Run a snap web service in the moonshine framework.
-}
runMoonshine :: Snap () -> IO ()
runMoonshine = quickHttpServe


{- |
  A version of `Snap.route` that autmatically sets up metrics for the specified routs.
-}
route :: MonadSnap m => [(ByteString, m a)] -> m a
route = error "implement me"

-- Private Types --------------------------------------------------------------
-- Private Functions ----------------------------------------------------------


