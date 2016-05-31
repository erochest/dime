{-# LANGUAGE TypeFamilies #-}


module Dime.Resume where


import qualified Control.Foldl          as L
import           Control.Monad.IO.Class
import           Data.Acid
import           Web.Twitter.Types

import           Dime.Types


-- | This steps through a @FoldM@, saving the state (a @DMCursor@) at each
-- step.
withResume :: (MonadIO m, Foldable f)
           => FilePath
           -> (CursorData -> a -> m CursorData)
           -> m CursorData
           -> (CursorData -> m DMCursor)
           -> f a
           -> m [DirectMessage]
withResume stateDir fstep finit ffinal inputs = do
    finit' <- finit
    acid   <- liftIO $ openLocalStateFrom stateDir $ uncurry DMCursor finit'

    let cursor     =   liftIO $ query acid QueryDMCursor
        fstep' s a = do
            s' <- fstep s a
            liftIO . update acid $ WriteDMCursor s'
            return s'

    _cursorDMs
        <$> L.foldM (L.FoldM fstep' cursor ffinal) inputs
        <*  liftIO (closeAcidState acid)
