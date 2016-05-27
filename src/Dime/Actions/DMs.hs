{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.DMs where


import           Control.Arrow                  ((&&&))
import           Control.Concurrent             (threadDelay)
import           Control.Error
import           Control.Lens                   hiding (from, to, (??))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops            (iterateUntilM)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy           as BL
import qualified Data.List                      as L
import           Network.HTTP.Conduit
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Conduit.Base
import           Web.Twitter.Conduit.Parameters
import           Web.Twitter.Conduit.Request
import           Web.Twitter.Conduit.Types
import           Web.Twitter.Types

import           Dime.Auth
import           Dime.Config
import           Dime.Types


scrapeDMs :: FilePath -> FilePath -> Script ()
scrapeDMs configFile output = do
    config <- readConfig configFile
    twInfo <- getTWInfo config ?? "You have to call 'dime login' first."

    manager <- scriptIO $ newManager tlsManagerSettings
    dms <- scriptIO . runResourceT $ do
        let to   = walkHistory twInfo manager dmId (directMessages & count ?~ 100)
                    >=> liftIO . sequenceA . second (write' "to.json")
            from = walkHistory twInfo manager dmId
                        (directMessagesSent & count ?~ 100)
                        >=> liftIO . sequenceA . second (write' "from.json")
            done = isCursorDone . fst
        short <- (++) <$> fmap snd (iterateUntilM done to   (NotStarted, []))
                      <*> fmap snd (iterateUntilM done from (NotStarted, []))
        mapM ( uncurry (*>)
             . (const throttle &&& liftIO . call twInfo manager . directMessagesShow . dmId)
             )
            short
    -- TODO: filter by friend

    scriptIO . write $ L.sortOn dmId dms
    where
        write = BL.writeFile output . encode
        write' fn xs = BL.writeFile fn (encode xs) >> return xs

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

