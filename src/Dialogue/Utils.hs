module Dialogue.Utils where


import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import           Debug.Trace
import           Text.Groom

import           Dialogue.Types


readInput :: TextInput -> IO T.Text
readInput (FileInput filename) = TIO.readFile filename
readInput (RawInput  msg)      = return msg
readInput StdInput             = TIO.getContents

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
