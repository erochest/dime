module Dime.Utils where


import           Control.Arrow
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.Text                 as T
import           Data.Time
import           Data.UUID                 (UUID)
import           Data.UUID.V1
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           Text.Groom
import           Web.Twitter.Conduit.Types
import           Web.Twitter.Types

import           Dime.Auth
import           Dime.Config
import           Dime.Types


walkDirectoryTree :: FilePath -> Script [FilePath]
walkDirectoryTree dirname = do
    (ds, fs) <-  scriptIO
             .   partitionM doesDirectoryExist
             .   map (dirname </>)
             .   filter (not . isHidden)
             =<< scriptIO (getDirectoryContents dirname)
    concat . (fs:) <$> mapM walkDirectoryTree ds
    where
        isHidden ('.':_) = True
        isHidden _       = False

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = foldrM step ([], [])
    where
        step x (ts, fs) = do
            r <- f x
            return $ if r
                        then (x:ts, fs)
                        else (ts, x:fs)

watchM :: (Monad m, Show a) => String -> a -> m a
watchM msg x = do
    traceM $ msg ++ groom x
    return x

watchFM :: Monad m => String -> (a -> String) -> a -> m a
watchFM msg f x = do
    traceM $ msg ++ f x
    return x

watchF :: Show b => String -> (a -> b) -> a -> a
watchF msg f x = trace (msg ++ groom (f x)) x

dedupDM :: [DirectMessage] -> [DirectMessage]
dedupDM = toList . M.fromList . fmap (dmId &&& id)

foldUntilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
foldUntilM p f x = do
    r <- p x
    if r
       then return x
       else foldUntilM p f =<< f x

readTWInfo :: FilePath -> Script TWInfo
readTWInfo = getTWInfo' <=< readConfig

getTWInfo' :: LoginInfo -> Script TWInfo
getTWInfo' = (?? "You have to call 'dime twitter-login' first.") . getTWInfo

bothA :: Applicative m => (m a, m b) -> m (a, b)
bothA (ma, mb) = (,) <$> ma <*> mb

boolM :: MonadPlus m => (a -> Bool) -> a -> m a
boolM p a
    | p a       = return a
    | otherwise = mzero

print' :: (MonadIO m, Show a) => a -> m ()
print'    = liftIO . print

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

rfc822Date :: UTCTime -> T.Text
rfc822Date = T.pack . formatTime defaultTimeLocale rfc822DateFormat

uuid :: MonadIO m => m UUID
uuid = do
    u' <- liftIO nextUUID
    case u' of
         Just u  -> return u
         Nothing -> liftIO (threadDelay 100) >> uuid

