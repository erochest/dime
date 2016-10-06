{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Utils where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as C8
import           Data.Foldable
import           Data.Monoid
import qualified Data.Sequence                as S
import qualified Data.Text                    as T
import           Data.Text.Format
import qualified Data.Text.Format             as F
import           Data.Text.Read
import           Database.Persist
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Groom
import           Web.Browser


unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM m = do
    a' <- m
    case a' of
        Just a  -> (a:) <$> unfoldM m
        Nothing -> return []

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ groom x) x

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

ifMaybe :: Bool -> x -> Maybe x
ifMaybe True  x = Just x
ifMaybe False _ = Nothing

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

over2 :: (Applicative f, Traversable (p c), Bifunctor p)
      => (b -> f a) -> p c b -> f (p c a)
over2 = sequenceA ... second

listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory dirname =   fmap (dirname </>)
                      .   filter ((/= ".") . take 1)
                      <$> liftIO (getDirectoryContents dirname)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p (x:xs) = do
    (ts, fs) <- partitionM p xs
    r <- p x
    return $ if r then (x:ts, fs) else (ts, x:fs)
partitionM _ [] = return ([], [])

walkDirectory :: MonadIO m => FilePath -> m [FilePath]
walkDirectory dirname = toList <$> go dirname
    where
        go dn = do
            (dirs, files) <-  partitionM (liftIO . doesDirectoryExist)
                          =<< listDirectory dn
            (S.fromList files <>) . mconcat <$> mapM go dirs

insertUniqueEntity :: ( MonadIO m
                      , PersistEntityBackend val ~ backend
                      , PersistUnique backend
                      , PersistEntity val)
                   => val -> ReaderT backend m (Maybe (Entity val))
insertUniqueEntity v = fmap (`Entity` v) <$> insertUnique v

getPIN :: String -> String -> ResourceT IO ByteString
getPIN provider uri = liftIO $ do
    F.print "Opening {}: <{}>\n" (provider, uri)
    void $ openBrowser uri
    F.print "> what was the PIN {} provided?" $ Only provider
    hFlush stdout
    C8.getLine

decimalE :: Integral a => T.Text -> Either String a
decimalE t = do
    (x, r) <- decimal t
    if T.null r
        then Right x
        else Left "Trailing text while parsing a number."
