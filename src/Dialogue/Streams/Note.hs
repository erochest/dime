{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


module Dialogue.Streams.Note where


import           Control.Arrow           ((&&&))
import           Control.Error
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Data
import qualified Data.List               as L
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           Data.Time
import           Data.Tuple              (swap)
import           Database.Persist
import           GHC.Generics
import           System.Directory
import           System.FilePath

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Types.Dialogue


newtype NoteException = NoteException { unNoteException :: T.Text }
                      deriving (Show, Eq, Data, Typeable, Generic)

instance Exception NoteException

data NoteStream
    = NoteStream
    { _noteServiceId :: !(Maybe ServiceInfoId)
    , _noteDirectory :: !FilePath
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''NoteStream)
$(makeFields ''NoteStream)

loadNote :: Dialogue NoteStream
loadNote =   hoistM ex . fmap entNote
         =<< liftSql
         (   selectFirst [ ServiceInfoService     ==. NoteService
                         , ServiceInfoIsActive    ==. True
                         , ServiceInfoAccessToken !=. Nothing
                         ]
                         [])
    where
        ex = NoteException "Invalid service: Note"
        entNote (Entity k v) = NoteStream (Just k)
                             . foldMap T.unpack
                             $ _serviceInfoAccessToken v

saveNote :: NoteStream -> Dialogue (ServiceInfoId, NoteStream)
saveNote ns@(NoteStream (Just sid) _ ) = return (sid, ns)
saveNote ns@(NoteStream Nothing    fp) =
    fmap (id &&& (flip (set noteServiceId) ns . Just))
        . liftSql
        . insert
        $ ServiceInfo NoteService False (Just $ T.pack fp) Nothing

lastNoteUpdateDate :: Dialogue (Maybe UTCTime)
lastNoteUpdateDate =   fmap (_noteMessageCreatedAt . entityVal)
                   <$> liftSql (selectFirst [] [Desc NoteMessageCreatedAt])

lastNoteUpdateID :: Dialogue (Maybe T.Text)
lastNoteUpdateID =   fmap (T.pack . _noteMessageFilePath . entityVal)
                 <$> liftSql (selectFirst [] [Desc NoteMessageCreatedAt])

loadNoteDirectory :: NoteStream -> Dialogue [Entity NoteMessage]
loadNoteDirectory (NoteStream _ dirname) = do
    messages <-  mapM readNote
             =<< liftIO
             .   filterM doesFileExist
             .   fmap (dirname </>)
             .   filter ((/= ".") . take 1)
             =<< liftIO
             (   getDirectoryContents dirname)
    ents <- fmap ( fmap (uncurry Entity . swap)
                 . mapMaybe sequenceA
                 . zip messages
                 )
         .  liftSql
         $  mapM insertUnique messages
    liftIO $ mapM_ ( removeFile
                   . (dirname </>)
                   . _noteMessageFilePath
                   . entityVal
                   ) ents
    return ents
    where
        readNote :: FilePath -> Dialogue NoteMessage
        readNote fp = do
            let fn = takeFileName fp
            createdAt <- parseNoteDate fn
            raw <- liftIO $ TIO.readFile fp
            return $ NoteMessage fn createdAt (parseNoteContent raw) raw

parseNoteDate :: Monad m => String -> m UTCTime
parseNoteDate = fmap (uncurry diffTime)
              . sequenceA
              . (parseOffset `bimap` parseTimeM False defaultTimeLocale format)
              . swap
              . L.break isSpace
              . capHead
              . takeBaseName
    where
        format = "%B_%d__%Y_at_%I%M%P"

        capHead :: String -> String
        capHead (x:xs) = toUpper x : xs
        capHead []     = []

        parseOffset :: String -> DiffTime
        parseOffset = secondsToDiffTime
                    . (* 6)
                    . fromMaybe 0
                    . readMay
                    . L.dropWhileEnd (== ')')
                    . L.dropWhile (== '(')
                    . L.dropWhile isSpace

        diffTime :: DiffTime -> UTCTime -> UTCTime
        diffTime dt (UTCTime d t) = UTCTime d $ t + dt

parseNoteContent :: T.Text -> T.Text
parseNoteContent =
    T.unlines . L.takeWhile (not . T.isPrefixOf "- - -") . T.lines

loadNoteMessages :: Dialogue [Entity NoteMessage]
loadNoteMessages = liftSql $ selectList [] [Asc NoteMessageCreatedAt]
