module Dialogue.Utils where


import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO

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
