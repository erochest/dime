{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dime.Actions.Counts where


import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               hiding (header)
import           Data.Foldable
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Time
import qualified Data.Vector            as V
import           Web.Twitter.Types.Lens

import           Dime.Twitter
import           Dime.Utils


twitterCounts :: FilePath -> FilePath -> Maybe T.Text -> Script ()
twitterCounts archiveFile outputFile mUser = do
    dms :: [DirectMessage] <-  hoistEither . eitherDecode'
                           =<< scriptIO (BL.readFile archiveFile)
    let i1      = indexBy (yearMonth . toGregorian . utctDay . view dmCreatedAt)
                $ filterByUser mUser dms
        counts  = fmap length <$> (indexBy (view dmSenderScreenName) <$> i1)
        senders = L.sort
                . toList
                . S.fromList
                . concatMap M.keys
                $ toList counts
        header  = V.fromList $ encodeUtf8 <$> ("year" : "month" : senders)

    liftIO . BL.writeFile outputFile . encodeByName header $
        uncurry (toRow header) <$> M.toList counts

    where
        yearMonth (y, m, _) = (y, m)

        tshow :: Show a => a -> T.Text
        tshow = T.pack . show

        toRow :: Header -> (Integer, Int) -> M.HashMap T.Text Int
              -> M.HashMap T.Text T.Text
        toRow h (year, month) = flip (foldl' insertDefault) h
                              . M.insert "year"  (tshow year)
                              . M.insert "month" (tshow month)
                              . fmap tshow

        insertDefault :: M.HashMap T.Text T.Text -> Name -> M.HashMap T.Text T.Text
        insertDefault m k = M.alter alter (decodeUtf8 k) m

        alter :: Maybe T.Text -> Maybe T.Text
        alter v@(Just _) = v
        alter Nothing    = Just ""
