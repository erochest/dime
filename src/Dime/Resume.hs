{-# LANGUAGE TypeFamilies #-}


module Dime.Resume where


import qualified Control.Foldl          as L
import           Control.Monad.IO.Class
import           Data.Acid
import           System.Directory
import           Web.Twitter.Types

import           Dime.Types


-- | This steps through a @FoldM@, saving the state (a @DMCursor@) at each
-- step.
withResumeFold :: (MonadIO m, Foldable f)
               => FilePath
               -> (CursorData -> a -> m CursorData)
               -> m CursorData
               -> (CursorData -> m DMCursor)
               -> f a
               -> m [DirectMessage]
withResumeFold stateDir fstep finit ffinal inputs = do
    finit' <- finit
    liftIO $ createDirectoryIfMissing True stateDir
    acid   <- liftIO $ openLocalStateFrom stateDir $ uncurry DMCursor finit'

    let cursor     =   liftIO $ query acid QueryDMCursor
        fstep' s a = do
            s' <- fstep s a
            liftIO . update acid $ WriteDMCursor s'
            return s'

    _cursorDMs
        <$> L.foldM (L.FoldM fstep' cursor ffinal) inputs
        <*  liftIO (createCheckpoint acid)

withResumeUntil :: MonadIO m
                => FilePath
                -> (CursorData -> Bool)
                -> (CursorData -> m CursorData)
                -> CursorData
                -> m CursorData
withResumeUntil stateDir isDone rstep rinit = do
    liftIO $ createDirectoryIfMissing True stateDir
    acid <- liftIO $ openLocalStateFrom stateDir $ uncurry DMCursor rinit
    resume acid rstep rinit <* liftIO (createCheckpoint acid)
    where
        resume :: MonadIO m
               => AcidState DMCursor
               -> (CursorData -> m CursorData)
               -> CursorData
               -> m CursorData
        resume acid rstep' current
            | isDone current = return current
            | otherwise      = do
                next <- rstep' current
                liftIO . update acid $ WriteDMCursor next
                resume acid rstep' next
