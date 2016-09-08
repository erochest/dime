{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Dime.Actions.Gmail where


import qualified Codec.Binary.QuotedPrintable as QP
import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens                 hiding ((??), (|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Csv                     as Csv
import           Data.Foldable
import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as S
import qualified Data.List                    as L
import           Data.List.Split
import           Data.Monoid
import           Data.Ord
import           Data.Sequence                ((|>))
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import qualified Data.Text.Lazy               as TL
import           Data.Text.Read
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.UUID                    (toText)
import qualified Data.Vector                  as V
import           Network.Mail.Mime
import           System.Environment
import           System.IO
import           Web.Twitter.Types.Lens

import           Dime.Google
import           Dime.Google.DSL              (batchActions, singleActions)
import qualified Dime.Google.Labels           as Labels
import qualified Dime.Google.Messages         as Messages
import           Dime.Google.Network
import qualified Dime.Google.Threads          as Threads
import           Dime.Types
import           Dime.Types.Google
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

archiveGmail :: FilePath -> FilePath -> FilePath -> LabelName -> FilePath
             -> Script ()
archiveGmail configFile userFile archive label workingDB =
    runGoogle' configFile workingDB $ do
        liftIO . TIO.putStrLn . mappend "You are: " =<< getUser

        twitterLabelId <- singleActions $ _labelId <$> Labels.ensure label
        print' twitterLabelId

        dumpSMS
        _ <- undefined

        messages <-  fmap (L.sortBy (comparing _messageInternalDate))
                 .   batchActions . mapM (Messages.get . _messageShortId)
                 =<< Messages.listAll [twitterLabelId] Nothing
        threads  <-  M.fromList . map (_threadId &&& id)
                 <$> Threads.listAll [twitterLabelId] Nothing
        tweets   <-  separateTwitterThreads <$> readJSON archive
        users    <-  fmap contactAddress <$> readJSON userFile

        mapM_ (   uncurry (importThread twitterLabelId threads messages users)
              <=< status)
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
        ms'  = filter ( any (uncurry inMessage)
                      . (<$> ([u1, u2] :: [UserId]))
                      . (,)
                      ) ms
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
        headers = fold $ m ^. messagePayload . payloadHeaders
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
    let smsLabel = mapMaybe (`M.lookup` labelIndex) ["SMS"]
    {-
     - sms <- singleActions $ Messages.list smsLabel (Just 25) Nothing Nothing
     - putStrLn' "SMS"
     - putStrLn' $ groom sms
     - putStrLn' . groom
     -     =<< batchActions
     -         (mapM (Messages.get . _messageShortId) $ _messagesMessages sms)
     -}

    email <- liftE . ExceptT . fmap (note "Missing QUERY_EMAIL.")
          $  lookupEnv "QUERY_EMAIL"
    putStrLn' ("Retrieving messages for " ++ email)
        >> liftIO (hFlush stdout)
    messages <- Messages.listAll' smsLabel . Just $ T.pack email

    putStrLn' "Indexing"
        >> liftIO (hFlush stdout)
    messageCounts <-  liftE
                  $   mapM (fmap (fmap length) . indexByM bySender)
                  =<< indexByM byMonth messages

    putStrLn' "Aggregating SMS by date-sender and writing to message-counts.csv"
        >> liftIO (hFlush stdout)
    let senders = fmap encodeUtf8 . L.sort . toList . S.fromList . concatMap M.keys
                $ toList messageCounts
        headers = V.fromList ("year":"month":senders)
    writeCSV "message-counts.csv" headers
             $ toRow headers <$> M.toList messageCounts
    putStrLn' "Writing date, sender, and message to messages.csv"
        >> liftIO (hFlush stdout)
    writeCSV "messages.csv"
             ["date", "sender", "message"]
             $ toMessageRow <$> messages
    where
        writeCSV :: Csv.ToNamedRecord a
                 => FilePath -> Csv.Header -> [a] -> Google ()
        writeCSV fp h = liftIO . BL.writeFile fp . Csv.encodeByName h

        toRow :: Csv.Header -> ((Integer, Int), M.HashMap T.Text Int)
              -> M.HashMap T.Text Integer
        toRow h ((y, m), r) = flip (foldl' insertDefault) h
                            . M.insert "year"  y
                            . M.insert "month" (fromIntegral m)
                            $ fromIntegral <$> r

        toMessageRow :: Message -> M.HashMap T.Text T.Text
        toMessageRow m =
            let pl  = m ^. messagePayload
                fmt = iso8601DateFormat (Just "%H:%M:%S")
                d   = maybe "" (T.pack . formatTime defaultTimeLocale fmt)
                    . hush
                    $ messageDate m
                s   = fold $ headerLU "From" =<< pl ^. payloadHeaders
                msg :: Maybe T.Text
                    =   ( pl ^? payloadBody . attachmentData . _Just
                        . to unBytes . to decodeUtf8)
                    <|> (   hush . fmap decodeUtf8 . QP.decode . unBytes
                        =<< preview (payloadBody . attachmentData . _Just)
                        =<< listToMaybe . filter isTextPart
                        =<< pl ^. payloadParts
                        )
            in  M.fromList [ ("date"   , d)
                           , ("sender" , s)
                           , ("message", fold msg)
                           ]

        insertDefault :: M.HashMap T.Text Integer -> Csv.Name
                      -> M.HashMap T.Text Integer
        insertDefault m k = M.alter alter (decodeUtf8 k) m

        alter :: Maybe Integer -> Maybe Integer
        alter v@(Just _) = v
        alter Nothing    = Just 0

        isTextPart :: Payload -> Bool
        isTextPart = any isTextHeader . fold . _payloadHeaders

        isTextHeader :: Header -> Bool
        isTextHeader Header{..} =  _headerName  == "Content-Type"
                                && _headerValue == "text/plain; charset=utf-8"

        messageDate :: Message -> Either String UTCTime
        messageDate = fmap (posixSecondsToUTCTime . realToFrac)
                    . (decimalE :: T.Text -> Either String Integer)
                    . _messageInternalDate

        byMonth :: Message -> Script (Integer, Int)
        byMonth = hoistEither
                . fmap ((\(y, m, _) -> (y, m)) . toGregorian . utctDay)
                . messageDate

        bySender :: Message -> Script T.Text
        bySender =   hoistEither
                 .   note "Missing 'From' header"
                 .   (headerLU "From" <=< _payloadHeaders)
                 .   _messagePayload

        headerLU :: T.Text -> [Header] -> Maybe T.Text
        headerLU k (Header{..}:hs)
            | _headerName == k = Just _headerValue
            | otherwise        = headerLU k hs
        headerLU _ [] = Nothing
        headerLU _ _  = Nothing

readJSON :: FromJSON a => FilePath -> Google a
readJSON =   liftE . hoistEither . eitherDecodeStrict'
         <=< liftIO . B.readFile

messageTweetId :: Message -> Maybe StatusId
messageTweetId m = listToMaybe
                 . rights
                 . map (fmap fst . decimal . _headerValue)
                 . filter ((== hTwitterId) . _headerName)
                 . fold
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
    PayloadInfo partType (fold partFilename) $ headers ++ fromHeaders partHeaders
