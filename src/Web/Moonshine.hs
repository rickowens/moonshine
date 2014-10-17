module Web.Moonshine (
  runMoonshine,
  route
) where

import Data.ByteString (ByteString)
import Snap (Snap, quickHttpServe, MonadSnap)
import qualified Snap (route)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------
-- Public Functions -----------------------------------------------------------

{- |
  Run a snap web service in the moonshine framework.
-}
runMoonshine :: (a -> Snap ()) -> IO ()
runMoonshine init =
  let config = undefined in
  quickHttpServe (init config)


{- |
  A version of `Snap.route` that autmatically sets up metrics for the specified routs.
-}
route :: MonadSnap m => [(ByteString, m a)] -> m a
route = Snap.route

-- Private Types --------------------------------------------------------------
-- Private Functions ----------------------------------------------------------


