{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Actions.Mail where


import           Codec.MIME.Parse
import           Codec.MIME.Type
import           Control.Arrow           ((&&&))
import           Control.Error
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.List.Split
import           Data.MBox
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.IO       as TLIO
import           Data.Time
import           Database.Persist

import           Dialogue.Models
import           Dialogue.Streams.Mail
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


importMBox :: FilePath -> FilePath -> Script ()
importMBox dbFile mboxFile = runDialogueS' (T.pack dbFile) $ do
    mbox <- parseMBox <$> liftIO (TLIO.readFile mboxFile)
    handleProfiles <-  liftSql
                   $   M.fromList
                   .   map ((_handleHandle . entityVal) &&& entityKey)
                   <$> selectList [] []
    mapM_ (   liftSql . insert
          <=< hoistE . first toException . parseMessage handleProfiles
          ) mbox

parseMessage :: M.HashMap T.Text HandleId -> Message
             -> Either MailException MailMessage
parseMessage index m = do
    cType <-  note (MailException "Invalid MIME type") . parseMIMEType
          =<< getHeaderF "Content-Type" m
    content <- case mimeType cType of
                    Multipart _ -> do
                        boundary <- fmap (mappend "--" . TL.fromStrict)
                                 .  note (MailException "no boundary")
                                 .  listToMaybe
                                 .  map paramValue
                                 .  filter ((== "boundary") . paramName)
                                 $  mimeParams cType
                        note (MailException "Invalid content")
                            . listToMaybe
                            . mapMaybe getContent
                            . filter (isTextPlain . mime_val_type)
                            . map (parseMIMEMessage . TL.toStrict . TL.unlines)
                            . filter (not . L.null)
                            . splitWhen (== boundary)
                            . map (TL.dropAround (== '\r'))
                            . TL.lines
                            $ body m
                    Text _ -> pure . TL.toStrict $ body m
                    _ -> pure ""
    MailMessage
        <$> pure (fold $ getHeaderM isID m)
        <*> (   parseTimeM True defaultTimeLocale rfc822DateFormat . T.unpack
            =<< getHeaderE "Date" isDate m)
        <*> getEmail "From" m index
        <*> getEmail "To"   m index
        <*> getHeaderF "Subject" m
        <*> pure content
        <*> pure (TL.toStrict $ showMessage m)
    where
        getContent :: MIMEValue -> Maybe T.Text
        getContent v
            | isTextPlain (mime_val_type v) = case c of
                                                   Single t -> Just t
                                                   _        -> Nothing
            | otherwise = case c of
                               Multi ts -> getFirst $ foldMap (First . getContent) ts
                               _ -> Nothing
            where
                c = mime_val_content v

        isTextPlain :: Type -> Bool
        isTextPlain Type{mimeType=(Text "plain")} = True
        isTextPlain _                             = False

        -- recode :: T.Text -> T.Text
        -- recode = decodeLatin1 . B8.filter (/= '\x1b') . B8.pack . T.unpack

(???) :: Maybe x -> T.Text -> Either MailException x
m ??? e = note (MailException e) m

getHeaderM :: (Header -> Bool) -> Message -> Maybe T.Text
getHeaderM = (fmap TL.toStrict . listToMaybe) ... getHeader

getHeaderE :: T.Text -> (Header -> Bool) -> Message
           -> Either MailException T.Text
getHeaderE f p m =  getHeaderM p m ??? ("Missing header: " <> f)

getHeaderF :: TL.Text -> Message -> Either MailException T.Text
getHeaderF f = getHeaderE (TL.toStrict f) (isHeader f)

getEmail :: TL.Text -> Message -> M.HashMap T.Text a -> Either MailException a
getEmail field m index = do
    value <- getHeaderF field m
    email <- (MailException . T.pack) `bimap` fmap snd $ parseEmail value
    listToMaybe (mapMaybe (`M.lookup` index) email)
            ??? mappend "Missing email link: " (TL.toStrict field)

isHeader :: TL.Text -> Header -> Bool
isHeader name = (== name) . fst
