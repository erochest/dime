{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}


module Dialogue.Streams.Adium where


import           Conduit
import           Control.Arrow           ((&&&), (***))
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Data.Data
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Builder
import           Data.Time
import           Data.XML.Types
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           System.Directory
import           System.FilePath
import qualified Text.XML.Stream.Parse   as XML

import           Dialogue.Fields
import           Dialogue.Handles
import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


newtype AdiumException = AdiumException { unAdiumException :: T.Text }
                       deriving (Show, Eq, Data, Typeable, Generic)

instance Exception AdiumException

data AdiumStream
    = AdiumStream
    { _adiumServiceId :: !(Maybe ServiceInfoId)
    , _adiumDirectory :: !FilePath
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''AdiumStream)
$(makeFields ''AdiumStream)

loadAdium :: Dialogue AdiumStream
loadAdium =   hoistM ex . fmap entAdium
          =<< liftSql
          (   selectFirst [ ServiceInfoService     ==. AdiumService
                          , ServiceInfoIsActive    ==. True
                          , ServiceInfoAccessToken !=. Nothing
                          ]
                          [])
    where
        ex = AdiumException "Invalid service: Adium"
        entAdium (Entity k v) = AdiumStream (Just k)
                              . foldMap T.unpack
                              $ _serviceInfoAccessToken v

saveAdium :: AdiumStream -> Dialogue (ServiceInfoId, AdiumStream)
saveAdium as@(AdiumStream (Just sid) ad) = do
    liftSql $ update sid [ ServiceInfoAccessToken =. Just (T.pack ad) ]
    return (sid, as)
saveAdium as@(AdiumStream Nothing    fp) =
    fmap (id &&& (flip (set adiumServiceId) as . Just))
        . liftSql
        . insert
        $ ServiceInfo AdiumService False (Just $ T.pack fp) Nothing

lastAdiumUpdateDate :: Dialogue (Maybe UTCTime)
lastAdiumUpdateDate =   fmap (_adiumMessageCreatedAt . entityVal)
                    <$> liftSql (selectFirst [] [Desc AdiumMessageCreatedAt])

lastAdiumUpdateID :: Dialogue (Maybe T.Text)
lastAdiumUpdateID =   fmap (_adiumMessageAdiumId . entityVal)
                 <$> liftSql (selectFirst [] [Desc AdiumMessageCreatedAt])

loadAdiumMessages :: Dialogue [Entity AdiumMessage]
loadAdiumMessages =
    liftSql $ selectList [] [Asc AdiumMessageCreatedAt]

data AdiumDialogue a
    = AD
    { adF :: !T.Text
    , adR :: !T.Text
    , adD :: !a
    } deriving (Show, Eq, Functor)

instance Applicative AdiumDialogue where
    (AD _ _ d1) <*> (AD f2 r2 d2) = AD f2 r2 $ d1 d2
    pure = AD "" ""

instance Foldable AdiumDialogue where
    foldMap f (AD _ _ x) = f x
    foldr f b (AD _ _ x) = f x b

instance Traversable AdiumDialogue where
    traverse f (AD u1 u2 x) = AD u1 u2 <$> f x
    sequenceA  (AD u1 u2 f) = AD u1 u2 <$> f

data MessageBuffer
    = MB
    { _mbSender    :: !HandleId
    , _mbRecipient :: !HandleId
    , _mbTime      :: !UTCTime
    , _mbBuffer    :: !Builder
    } deriving (Show)
$(makeLenses ''MessageBuffer)

type ParseState = Maybe MessageBuffer

getNewAdiumMessages :: AdiumStream -> Dialogue [Entity AdiumMessage]
getNewAdiumMessages (AdiumStream (Just _) adiumDir)= do
    handles <- indexHandles AdiumService
    ircDirs <- directoryHandles handles
                (((== "IRC") *** (T.pack . drop 1)) . L.break (== '.'))
                adiumDir
    msgXML  <-  filterM (liftIO . doesFileExist . adD)
            .   filter ((== ".xml") . takeExtension . adD)
            .   map toAD
            .   concatMap (sequenceA . fmap sequenceA)
            =<< mapM (over2 (over2 walkDirectory))
            .   concatMap sequenceA
            =<< mapM (over2 (directoryHandles handles ((True,) . T.pack)))
                     ircDirs
    -- TODO: This can all be one pipeline.
    liftSql
        .   fmap catMaybes
        .   mapM insertUniqueEntity
        .   concat
        =<< runResourceT (mapM (docMessages handles) msgXML)

    where
        toAD (a, (b, c)) = AD a b c

        docMessages :: (MonadIO m, MonadResource m)
                    => HandleIndex -> AdiumDialogue FilePath -> m [AdiumMessage]
        docMessages idx ad@(AD _ _ fn) =
            XML.parseFile XML.def fn
                =$= concatMapAccumC (step idx ad) Nothing
                $$  sinkList

        contentText :: [Content] -> T.Text
        contentText = foldMap content

        content (ContentText t)   = t
        content (ContentEntity e) = "&" <> e <> ";"

        step :: HandleIndex -> AdiumDialogue b -> Event -> ParseState
             -> (ParseState, [AdiumMessage])

        step hidx ad (EventBeginElement "{http://purl.org/net/ulf/ns/0.4-02}message" as) Nothing =
            let next = do
                    (sender, recipient) <- getUsers hidx ad as
                    time    <- lookup "time" as
                    utcTime <- parseTimeM False defaultTimeLocale
                                          (iso8601DateFormat $ Just "%H:%M:%S%z")
                                          . T.unpack $ contentText time
                    return $ MB sender recipient utcTime mempty
            in  (next , [])
        step _ _ (EventBeginElement _ _) ps = (ps, [])

        step _ _ (EventEndElement "{http://purl.org/net/ulf/ns/0.4-02}message") (Just mb) = (Nothing, [toMessage mb])
        step _ _ (EventEndElement _) ps = (ps, [])

        step _ _ (EventContent c) (Just mb) =
            (Just $ mb & mbBuffer <>~ fromText (contentText [c]), [])
        step _ _ (EventContent _) Nothing   = (Nothing, [])

        step _ _ (EventCDATA c) (Just mb) =
            (Just $ mb & mbBuffer <>~ fromText c, [])
        step _ _ (EventCDATA _) Nothing   = (Nothing, [])

        step _ _ EventBeginDocument ps  = (ps, [])
        step _ _ EventEndDocument ps    = (ps, [])
        step _ _ EventBeginDoctype{} ps = (ps, [])
        step _ _ EventEndDoctype ps     = (ps, [])
        step _ _ EventInstruction{} ps  = (ps, [])
        step _ _ EventComment{} ps      = (ps, [])

        toMessage :: MessageBuffer -> AdiumMessage
        toMessage (MB s r t b) =
            let t'  = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
                mid = T.pack (show (fromSqlKey s)) <> "-" <> T.pack t'
            in  AdiumMessage mid t s r . TL.toStrict $ toLazyText b

        getUsers :: HandleIndex -> AdiumDialogue a -> [(Name, [Content])]
                 -> Maybe (HandleId, HandleId)
        getUsers idx (AD u1 u2 _) as = do
            sender <- contentText <$> lookup "sender" as
            let recipient = if sender == u1 then u2 else u1
            (,) <$> M.lookup sender idx <*> M.lookup recipient idx

        directoryHandles :: MonadIO m
                         => HandleIndex
                         -> (FilePath -> (Bool, T.Text))
                         -> FilePath
                         -> m [(T.Text, FilePath)]
        directoryHandles handleIndex checkGetHandle dirname =
            liftIO
                $   filterM (doesDirectoryExist . snd)
                .   map (fmap (dirname </>))
                .   mapMaybe ( join . fmap (uncurry getHandleId) . sequenceA
                             . (id &&& (uncurry ifMaybe . checkGetHandle)))
                =<< getDirectoryContents dirname
            where
                getHandleId :: a -> T.Text -> Maybe (T.Text, a)
                getHandleId x n | n `M.member` handleIndex = Just (n, x)
                                | otherwise                = Nothing

getNewAdiumMessages as@(AdiumStream Nothing _) =
    getNewAdiumMessages . snd =<< saveAdium as
