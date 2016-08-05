{-# LANGUAGE FlexibleContexts #-}


module Dime.Twitter where


import           Control.Concurrent             (threadDelay)
import           Control.Error
import           Control.Lens                   hiding (from, to, (??))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy           as B
import qualified Data.List                      as L
import qualified Data.Text                      as T
import           Network.HTTP.Conduit
import           System.FilePath
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Conduit.Base
import           Web.Twitter.Conduit.Parameters
import           Web.Twitter.Conduit.Request
import           Web.Twitter.Conduit.Types
import           Web.Twitter.Types

import           Dime.Types
import           Dime.Utils


downloadDMs :: TWInfo -> Script [DirectMessage]
downloadDMs twInfo = scriptIO $ do
    manager <- newManager tlsManagerSettings
    runResourceT $
        let to   = walkHistory twInfo manager dmId
                        (directMessages & count ?~ 100 & fullText ?~ True)
            from = walkHistory twInfo manager dmId
                        (directMessagesSent & count ?~ 100 & fullText ?~ True)
            done = isCursorDone . fst
        in  (++) <$> fmap snd (foldUntilM (return . done) to   (NotStarted, []))
                 <*> fmap snd (foldUntilM (return . done) from (NotStarted, []))

walkHistory :: ( Monad m
               , MonadResource m
               , MonadIO m
               , FromJSON a
               , HasMaxIdParam (APIRequest apiName [a])
               )
            => TWInfo
            -> Manager
            -> (a -> Integer)
            -> APIRequest apiName [a]
            -> (IdCursor, [a])
            -> m (IdCursor, [a])
walkHistory twInfo manager getId apireq (mMaxId, accum) = do
    throttle
    liftIO . putStrLn $ "retrieving with max ID " ++ show mMaxId
    res <- liftIO . call twInfo manager $ apireq & maxId .~ cursorDoneMaybe mMaxId
    case res of
      [] -> return (CursorDone, accum)
      _  -> let nextId = minimum $ fmap getId res
             in return (Cursor (nextId - 1), res ++ accum)

throttle :: MonadIO m => m ()
throttle = liftIO . threadDelay . floor $ (60 * 1e6 ::Double)

sortByDate :: [DirectMessage] -> [DirectMessage]
sortByDate = L.sortOn dmCreatedAt

filterByUser :: Maybe T.Text -> [DirectMessage] -> [DirectMessage]
filterByUser (Just u) xs = filter (involvesUser u) xs
filterByUser Nothing  xs = xs

involvesUser :: T.Text -> DirectMessage -> Bool
involvesUser n dm =  userScreenName (dmSender    dm) == n
                  || userScreenName (dmRecipient dm) == n

readDMDirTree :: FilePath -> Script [DirectMessage]
readDMDirTree =   fmap concat
              .   mapM (hoistEither . eitherDecode' <=< scriptIO . B.readFile)
              .   filter ((== ".json") . takeExtension)
              <=< walkDirectoryTree
