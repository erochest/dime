module Dime.Utils where


import           Control.Arrow
import           Control.Error
import           Control.Monad             ((<=<))
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           Text.Groom
import           Web.Twitter.Conduit.Types
import           Web.Twitter.Types

import           Dime.Auth
import           Dime.Config
import           Dime.Types


putStrLn' :: String -> Script ()
putStrLn' = scriptIO . putStrLn

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

getTWInfo' :: LoginInfo s -> Script TWInfo
getTWInfo' = (?? "You have to call 'dime login' first.") . getTWInfo

bothA :: Applicative m => (m a, m b) -> m (a, b)
bothA (ma, mb) = (,) <$> ma <*> mb
