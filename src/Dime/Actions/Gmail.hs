{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Dime.Actions.Gmail where


import           Control.Arrow
import           Control.Error
import           Control.Lens           hiding ((??), (|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import qualified Data.List              as L
import           Data.List.Split
import           Data.Monoid
import           Data.Ord
import           Data.Sequence          ((|>))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Text.Read
import           Data.Time
import           Data.UUID              (toText)
import           Network.Mail.Mime
import           Text.Groom
import           Web.Twitter.Types.Lens

import           Dime.Google
import           Dime.Google.DSL        (batchActions, singleActions)
import qualified Dime.Google.Labels     as Labels
import qualified Dime.Google.Messages   as Messages
import           Dime.Google.Network
import qualified Dime.Google.Threads    as Threads
import           Dime.Google.Types
import           Dime.Utils


type UserIndex     = M.HashMap T.Text Address
type ThreadIndex   = M.HashMap ThreadId Thread
type TwitterThread = ((UserId, UserId), [DirectMessage])

maxThreadSize :: Int
maxThreadSize = 100

hTwitterId, hTwitterBackupTime, hTwitterSender, hTwitterRecipient :: T.Text

hTwitterId         = "X-dime-Twitter-ID"
hTwitterBackupTime = "X-dime-Twitter-backup-time"
hTwitterSender     = "X-dime-Twitter-sender"
hTwitterRecipient  = "X-dime-Twitter-recipient"

hTwitterId', hTwitterBackupTime', hTwitterSender'
    , hTwitterRecipient' :: B.ByteString

hTwitterId'         = encodeUtf8 hTwitterId
hTwitterBackupTime' = encodeUtf8 hTwitterBackupTime
hTwitterSender'     = encodeUtf8 hTwitterSender
hTwitterRecipient'  = encodeUtf8 hTwitterRecipient

archiveGmail :: FilePath -> FilePath -> FilePath -> LabelName -> Script ()
archiveGmail configFile userFile archive label = runGoogle' configFile $ do
    liftIO . TIO.putStrLn . mappend "You are: " =<< getUser

    twitterLabelId <- singleActions $ _labelId <$> Labels.ensure label
    print' twitterLabelId

    dumpSMS

    messages <-  fmap (L.sortBy (comparing _messageInternalDate))
             .   batchActions . mapM (Messages.get . _messageShortId)
             =<< Messages.listAll [twitterLabelId] Nothing
    threads  <-  M.fromList . map (_threadId &&& id)
             <$> Threads.listAll [twitterLabelId] Nothing
    tweets   <-  separateTwitterThreads <$> readJSON archive
    users    <-  fmap contactAddress <$> readJSON userFile

    mapM_ (uncurry (importThread twitterLabelId threads messages users) <=< status)
          tweets
    where
        status p@((u1, u2), dms)
            =  putStrLn' (  "Importing conversation between "
                         ++ show u1 ++ " and " ++ show u2 ++ "."
                         ++ show (length dms) ++ " total tweets."
                         )
            >> return p

importThread :: LabelId
             -> ThreadIndex
             -> [Message]
             -> UserIndex
             -> (UserId, UserId)
             -> [DirectMessage]
             -> Google ()
importThread label ts ms ui (u1, u2) dms = do
    let toLoad =   fromMaybe dms'
               $   tailZ . (`L.dropWhile` dms') . (. view dmId) . (/=)
               =<< messageTweetId
               =<< snd <$> tm
    putStrLn' $ show (length toLoad) ++ " to load."
    email <- catMaybes <$> mapM (mail label ui) toLoad
    mapM_ (uncurry insertThread) $ twitterThreads (fst <$> tm) email
    where
        dms' = L.sortBy (comparing (view dmCreatedAt)) dms
        ms'  = filter (any (uncurry inMessage) . (<$> [u1, u2]) . (,)) ms
        mts  = S.fromList $ _messageThreadId <$> ms'
        tm :: Maybe (Thread, Message)
        tm   = lastZ
             . mapMaybe (sequenceA . (id &&& lastMessage))
             . filter ((< maxThreadSize) . length . _threadMessages)
             . toList
             $ M.filterWithKey (const . (`S.member` mts)) ts

lastMessage :: Thread -> Maybe Message
lastMessage =   lastZ . L.sortBy (comparing _messageInternalDate)
            <=< _threadMessages

inMessage :: Message -> UserId -> Bool
inMessage m uid =  Header hTwitterSender uid'    `elem` headers
                || Header hTwitterRecipient uid' `elem` headers
    where
        headers = m ^. messagePayload . payloadHeaders
        uid'    = T.pack $ show uid

separateTwitterThreads :: [DirectMessage] -> [TwitterThread]
separateTwitterThreads = L.sortBy (comparing fst)
                       . fmap (fmap toList)
                       . M.toList
                       . M.fromListWith mappend
                       . fmap (getUsers &&& Seq.singleton)
    where
        getUsers dm = order (dm ^. dmSender    . userId)
                            (dm ^. dmRecipient . userId)
        order a b | a <= b    = (a, b)
                  | otherwise = (b, a)

twitterThreads :: Maybe Thread -> [a] -> [(Maybe (ThreadId, MessageId), [a])]
twitterThreads (Just t) = uncurry (:)
                        . ((getThreadMessageId t,) *** chunkMaxThreadSize)
                        . L.splitAt (maxThreadSize - length (_threadMessages t))
twitterThreads Nothing  = chunkMaxThreadSize

chunkMaxThreadSize :: [a] -> [(Maybe b, [a])]
chunkMaxThreadSize = fmap (Nothing,) . chunksOf maxThreadSize

getThreadMessageId :: Thread -> Maybe (ThreadId, MessageId)
getThreadMessageId t =
    sequenceA (_threadId t, fmap _messageId . lastZ =<< _threadMessages t)

insertThread :: Maybe (ThreadId, MessageId) -> [(MessageInfo, Mail)]
             -> Google [Message]
insertThread current ms = toList . snd <$> foldlM step (current, Seq.empty) ms
    where
        step :: (Maybe (ThreadId, MessageId), Seq.Seq Message)
             -> (MessageInfo, Mail)
             -> Google (Maybe (ThreadId, MessageId), Seq.Seq Message)
        step (t, accum) (mi, m) = do
            let (mi', m') = thread t mi m
            message <-  singleActions
                    .   Messages.insert (watchF "INSERTING " (dumpMI m') mi')
                    =<< toRaw mi' m'
            return (Just (message ^. messageThreadId, message ^. messageId)
                   , accum |> message
                   )

        dumpMI :: Mail -> MessageInfo
               -> (Maybe ThreadId, T.Text, T.Text, B.ByteString)
        dumpMI m mi = ( mi ^. messageInfoThreadId
                      , renderAddress (mailFrom m)
                      , T.intercalate ", " (renderAddress <$> mailTo m)
                      , B.intercalate "\n---\n"
                          . fmap (BL.toStrict . partContent)
                          . concat
                          $ mailParts m
                      )

        thread :: Maybe (ThreadId, MessageId) -> MessageInfo -> Mail
               -> (MessageInfo, Mail)
        thread Nothing mi m = (mi, m)
        thread (Just (tid, mid)) mi m =
            ( mi & messageInfoPayload . payloadInfoHeaders <>~
                                            [Header "References" mid]
                 & messageInfoThreadId ?~ tid
            , m { mailHeaders = mailHeaders m <>
                                            [("References", mid)]
                }
            )

        toRaw :: MessageInfo -> Mail -> Google RawMessage
        toRaw MessageInfo{_messageInfoLabelIds=l,_messageInfoThreadId=t} m = do
            m' <- BL.toStrict <$> liftIO (renderMail' m)
            return $ RawMessage (JSBytes m') l t

dumpSMS :: Google ()
dumpSMS = do
    labelIndex <- singleActions Labels.index
    sms <- singleActions
        $  Messages.list (mapMaybe (`M.lookup` labelIndex) ["SMS"]) (Just 25)
                         Nothing Nothing
    putStrLn' "SMS"
    putStrLn' . groom
        =<< batchActions
            (mapM (Messages.get . _messageShortId) $ _messagesMessages sms)

readJSON :: FromJSON a => FilePath -> Google a
readJSON =   liftE . hoistEither . eitherDecodeStrict'
         <=< liftIO . B.readFile

messageTweetId :: Message -> Maybe StatusId
messageTweetId m = listToMaybe
                 . rights
                 . map (fmap fst . decimal . _headerValue)
                 . filter ((== hTwitterId) . _headerName)
                 $ m ^. messagePayload . payloadHeaders

luEmail :: T.Text -> M.HashMap T.Text Address -> T.Text
luEmail k = maybe k addressEmail . M.lookup k

contactAddress :: Contact -> Address
contactAddress (Contact n e) = Address (Just n) e

newMessageId :: Google MessageId
newMessageId =
    (<> "@twitter-dm.dime.local") . T.cons '<' . toText <$> liftIO uuid

mail :: LabelId -> UserIndex -> DirectMessage
     -> Google (Maybe (MessageInfo, Mail))
mail l ui dm = do
    mid <-  newMessageId
    now <-  rfc822Date <$> liftIO getCurrentTime

    return $ do
        sender <- M.lookup (dm ^. dmSenderScreenName)   ui
        recp   <- M.lookup (dm ^. dmRecipientScreeName) ui

        let subj =  "Twitter DM with "
                 <> fromMaybe (addressEmail sender) (addressName sender)
            dimeHeaders =
                [ ("Message-ID", mid)
                , (hTwitterBackupTime', now)
                , (hTwitterId', T.pack . show $ dm ^. dmId)
                , ("Date", rfc822Date $ dm ^. dmCreatedAt)
                , ( hTwitterSender'
                  , T.pack . show $ dm ^. dmSender    . userId)
                , ( hTwitterRecipient'
                  , T.pack . show $ dm ^. dmRecipient . userId)
                ]
            m' = simpleMail' recp sender subj . TL.fromStrict $ dm ^. dmText
            m = m' { mailHeaders = mailHeaders m' ++ dimeHeaders }
            hds =  [ Header "To"   $ renderAddress recp
                   , Header "From" $ renderAddress sender
                   ]
                ++ fromHeaders (mailHeaders m)

        mi <-  fmap ( flip (MessageInfo Nothing (Just [l])) Nothing
                    . partToPayload hds)
           .   listToMaybe
           .   concat
           $   mailParts m

        return (mi, m)

fromHeaders :: Headers -> [Header]
fromHeaders = fmap (uncurry Header . first decodeUtf8)

partToPayload :: [Header] -> Part -> PayloadInfo
partToPayload headers Part{..} =
    PayloadInfo partType (fold partFilename) headers' [attachment] []
    where
        headers'   = headers ++ fromHeaders partHeaders
        content    = B64.encode $ BL.toStrict partContent
        attachment = AttachmentInfo (Just $ B.length content) $ JSBytes content
