module Dime.Utils where


import           Control.Arrow
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.Sequence             as S
import qualified Data.Text                 as T
import           Data.Text.Read
import           Data.Time
import           Data.Tuple                (swap)
import           Data.UUID                 (UUID)
import           Data.UUID.V1
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           Text.Groom
import           Web.Twitter.Types


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

watchFM :: (Monad m, Show b) => String -> (a -> b) -> a -> m a
watchFM msg f x = do
    traceM $ msg ++ groom (f x)
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

indexBy :: (Functor t, Foldable t, Hashable k, Eq k)
        => (a -> k) -> t a -> M.HashMap k [a]
indexBy f = fmap toList . foldl' insert M.empty . fmap (f &&& S.singleton)
    where
        insert m (k, xs) = M.insertWith mappend k xs m

indexByM :: (Traversable t, Foldable t, Hashable k, Eq k, Monad m)
         => (a -> m k) -> t a -> m (M.HashMap k [a])
indexByM f = fmap (fmap toList . foldl' insert M.empty)
           . mapM (fmap swap . sequenceA . (S.singleton &&& f))
    where
        insert m (k, xs) = M.insertWith mappend k xs m

decimalE :: Integral a => T.Text -> Either String a
decimalE t = do
    (x, r) <- decimal t
    if T.null r
       then Left  "Trailing text while parsing a number."
       else Right x

decimalP :: (Integral a, MonadPlus m) => T.Text -> m a
decimalP = either (const mzero) return . decimalE
