module Dime.Utils where


import           Control.Arrow
import           Control.Error
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           Text.Groom
import           Web.Twitter.Types


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
