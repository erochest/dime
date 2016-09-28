{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}


module Dialogue.Streams.Twitter where


import           Control.Concurrent             (threadDelay)
import           Control.Error
import           Control.Exception.Safe
import           Control.Lens                   hiding ((??))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor                 (first)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy           as BL
import           Data.Data
import qualified Data.HashMap.Strict            as M
import           Data.Monoid
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.Format               as F
import           Data.Time
import           Database.Persist               hiding (count)
import           GHC.Generics                   hiding (to)
import           Network.HTTP.Conduit
import           System.Environment
import           Web.Authenticate.OAuth         hiding (insert)
import qualified Web.Authenticate.OAuth         as OAuth
import           Web.Twitter.Conduit            hiding (count, maxId, sinceId,
                                                 update)
import           Web.Twitter.Conduit.Parameters
import           Web.Twitter.Types              hiding (Entity)

import           Dialogue.Fields
import           Dialogue.Handles
import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


newtype TwitterException = TwitterException { unTwitterException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception TwitterException

decodeJSON :: FromJSON a => ByteString -> Dialogue a
decodeJSON = hoistE
           . first (toException . TwitterException . T.pack)
           . eitherDecodeStrict'

foldUntilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
foldUntilM p f x = do
    r <- p x
    if r
        then return x
        else foldUntilM p f =<< f x

getEntity :: forall record (m :: * -> *)
          .  ( PersistStore (PersistEntityBackend record), PersistEntity record
             , MonadIO m)
          => Key record
          -> ReaderT (PersistEntityBackend record) m (Maybe (Entity record))
getEntity k = fmap (Entity k) <$> get k

toTM :: M.HashMap T.Text HandleId -> DirectMessage
     -> Maybe TwitterMessage
toTM idx dm@DirectMessage{..} = do
    senderId <- M.lookup dmSenderScreenName   idx
    recipId  <- M.lookup dmRecipientScreeName idx
    return $ TwitterMessage (fromIntegral dmId) dmCreatedAt
                            dmSenderScreenName   senderId
                            dmRecipientScreeName recipId
                            dmText
                            $ decodeUtf8 . BL.toStrict $ encode dm

insertDMs :: M.HashMap T.Text HandleId
          -> [DirectMessage]
          -> Dialogue [Entity TwitterMessage]
insertDMs idx =   report
              .   catMaybes
              <=< liftSql
              .   mapM insertUniqueEntity
              .   mapMaybe (toTM idx)
    where
        report xs = do
            F.print "{} Direct Messages added.\n" $ Only (length xs)
            return xs

data TwitterStream
    = TwitterStream
    { _twitterServiceId :: !(Maybe ServiceInfoId)
    , _twitterAccess    :: !(ByteString, ByteString)
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''TwitterStream)
$(makeFields ''TwitterStream)

data IdCursor
    = NotStarted
    | Cursor Integer
    | CursorDone
    deriving (Eq, Show, Data, Typeable, Generic)

cursorDoneMaybe :: IdCursor -> Maybe Integer
cursorDoneMaybe (Cursor i) = Just i
cursorDoneMaybe _          = Nothing

isCursorDone :: IdCursor -> Bool
isCursorDone CursorDone = True
isCursorDone _          = False

loadTwitter :: Dialogue TwitterStream
loadTwitter = do
    s <- liftSql
      $  selectFirst [ ServiceInfoService      ==. TwitterService
                     , ServiceInfoIsActive     ==. True
                     , ServiceInfoAccessToken  !=. Nothing
                     , ServiceInfoAccessSecret !=. Nothing
                     ] []
    maybe (throwD ex) return $ do
        (Entity sid s') <- s
        token  <- encodeUtf8 <$> _serviceInfoAccessToken  s'
        secret <- encodeUtf8 <$> _serviceInfoAccessSecret s'
        return $ TwitterStream (Just sid) (token, secret)
    where
        ex = TwitterException "Invalid service: Twitter"

saveTwitter :: TwitterStream -> Dialogue (ServiceInfoId, TwitterStream)
saveTwitter ts@(TwitterStream (Just sid) (t, s)) = do
    liftSql $ update sid [ ServiceInfoAccessToken  =. Just (decodeUtf8 t)
                         , ServiceInfoAccessSecret =. Just (decodeUtf8 s)
                         ]
    return (sid, ts)
saveTwitter ts@(TwitterStream Nothing    (t, s)) = do
    k <- liftSql
        . insert
        $ ServiceInfo TwitterService False
                      (Just (decodeUtf8 t)) (Just (decodeUtf8 s))
    return (k, ts & twitterServiceId ?~ k)

lastTwitterUpdateDate :: Dialogue (Maybe UTCTime)
lastTwitterUpdateDate =
    liftSql $   fmap (_twitterMessageCreatedAt . entityVal)
            <$> selectFirst [] [Desc TwitterMessageCreatedAt]

lastTwitterUpdateID :: Dialogue (Maybe Integer)
lastTwitterUpdateID =
    liftSql $   fmap (fromIntegral . _twitterMessageTwitterId . entityVal)
            <$> selectFirst [] [Desc TwitterMessageTwitterId]

getTwitterMessages :: Dialogue [Entity TwitterMessage]
getTwitterMessages = liftSql $ selectList [] [Asc TwitterMessageCreatedAt]

migrateDirectMessages :: ByteString -> Dialogue [Entity TwitterMessage]
migrateDirectMessages input = do
    ms  :: [DirectMessage] <- decodeJSON input
    idx <- indexHandles TwitterService
    insertDMs idx ms

migrateTwitterMessages :: ByteString -> Dialogue [Entity TwitterMessage]
migrateTwitterMessages input = do
    ms :: [TwitterMessage] <- decodeJSON input
    liftSql $ catMaybes <$> mapM insertUniqueEntity ms

twitterOAuth' :: Dialogue OAuth
twitterOAuth' = do
    tKey <- hoistM' (TwitterException "Missing TWITTER_KEY")    . liftIO
         $  lookupEnv "TWITTER_KEY"
    tSec <- hoistM' (TwitterException "Missing TWITTER_SECRET") . liftIO
         $  lookupEnv "TWITTER_SECRET"
    return $ twitterOAuth
           { oauthConsumerKey    = C8.pack tKey
           , oauthConsumerSecret = C8.pack tSec
           }

twitterCredential :: TwitterStream -> Credential
twitterCredential ts = Credential
                     [ ("oauth_token",        t)
                     , ("oauth_token_secret", s)
                     ]
    where
        (t, s) = ts ^. twitterAccess

makeTWInfo :: TwitterStream -> Dialogue TWInfo
makeTWInfo ts = do
    token <- (`TWToken` twitterCredential ts) <$> twitterOAuth'
    return $ TWInfo token Nothing

downloadDMs :: TwitterStream -> Maybe Integer -> Dialogue [DirectMessage]
downloadDMs ts updatedId = do
    twInfo  <- makeTWInfo ts
    manager <- liftIO $ newManager tlsManagerSettings
    runResourceT $
        let t      = walkHistory twInfo manager dmId
                   $ directMessages & count ?~ 100 & fullText ?~ True
                                    & maybe id (sinceId ?~) updatedId
            f      = walkHistory twInfo manager dmId
                   $ directMessagesSent & count ?~ 100 & fullText ?~ True
                                        & maybe id (sinceId ?~) updatedId
            done   = isCursorDone . fst
        in  (++) <$> fmap snd (foldUntilM (return . done) t (NotStarted, []))
                 <*> fmap snd (foldUntilM (return . done) f (NotStarted, []))

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
    F.print "Getting DMs from {} ({} so far).\n" (Shown mMaxId, length accum)
    res <- liftIO . call twInfo manager
        $  apireq & maxId .~ cursorDoneMaybe mMaxId
    case res of
        [] -> return (CursorDone, accum)
        _  -> let nextId = minimum $ fmap getId res
              in  return (Cursor (nextId - 1), res ++ accum)

throttle :: MonadIO m => m ()
throttle = liftIO . threadDelay $ floor (60 * 1e6 :: Double)

downloadTwitterMessages :: TwitterStream -> Dialogue [Entity TwitterMessage]
downloadTwitterMessages ts = do
    idx    <- indexHandles TwitterService
    lastId <- lastTwitterUpdateID
    insertDMs idx =<< downloadDMs ts lastId

loginTwitter :: Dialogue (ByteString, ByteString)
loginTwitter = do
    oauth'  <- twitterOAuth'
    manager <- liftIO $ newManager tlsManagerSettings
    let oauth = oauth' { oauthCallback = Just "oob" }
    Credential cred <- liftIO . runResourceT $ do
        cred <- getTemporaryCredential oauth manager
        pin  <- getPIN "Twitter" $ authorizeUrl oauth cred
        getAccessToken oauth (OAuth.insert "oauth_verifier" pin cred) manager
    liftE $ (,) <$> luToken "oauth_token"        cred
                <*> luToken "oauth_token_secret" cred
    where
        luToken k xs =  Prelude.lookup (encodeUtf8 k) xs
                     ?? toException (TwitterException $ "Missing " <> k)
