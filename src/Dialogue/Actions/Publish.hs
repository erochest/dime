{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}


module Dialogue.Actions.Publish where


import           Control.Arrow           hiding (first)
import           Control.Error
import           Control.Exception.Safe
import           Control.Lens            hiding ((:<), (<.>), (|>))
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable
import           Data.Function           (on)
import qualified Data.HashMap.Strict     as M
import           Data.Int
import qualified Data.List               as L
import           Data.Monoid
import           Data.Ord                (comparing)
import           Data.Sequence           (ViewL (..), (|>))
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import           Data.Text.Format
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO       as TLIO
import           Data.Time
import           Database.Persist
import           Database.Persist.Sql
import           System.Directory
import           System.FilePath
import           Text.Pandoc

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Types.Publish
import           Dialogue.Utils
import           Paths_dialogue


blockSpan :: NominalDiffTime
blockSpan = 900
-- 15 minutes

publishEpub :: FilePath -> FilePath -> Script ()
publishEpub dbFile outputDir = runDialogueS' (T.pack dbFile) $ do
    as :: [AdiumMessage  ] <- liftSql $ map entityVal <$> selectList [] []
    gs :: [GoogleMessage ] <- liftSql $ map entityVal <$> selectList [] []
    js :: [Journal       ] <- liftSql $ map entityVal <$> selectList [] []
    ns :: [NoteMessage   ] <- liftSql $ map entityVal <$> selectList [] []
    ts :: [TwitterMessage] <- liftSql $ map entityVal <$> selectList [] []

    ps <-  liftSql $ M.fromList . map ((fromSqlKey . entityKey) &&& id)
       <$> selectList [] []
    let p0 = fold $ snd <$> getProfile (_profilePrimary . entityVal) (M.elems ps)

    hs <-  liftSql
       $   M.fromList
       .   map (fmap (_profileNickname . entityVal))
       .   mapMaybe ( sequenceA
                    . (   (fromSqlKey . entityKey)
                      &&& ( (`M.lookup` ps) . fromSqlKey . _handleProfileId
                          . entityVal)
                      ))
       <$> selectList [] []

    let tob :: forall t p. (Functor t, Publishable p) => t p -> t PublishBlock
        tob = fmap (toBlock hs p0)
        as' = tob as
        gs' = tob gs
        js' = tob js
        ns' = tob ns
        ts' = tob ts
        bs  = L.groupBy ((==) `on` (monthKey . _pbDate))
            $ L.sortBy (comparing _pbDate)
            $ mconcat [as', gs', js' , ns', ts']

    liftIO $ createDirectoryIfMissing True outputDir
    now <- liftIO $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
    let basename = "dialogue-" ++ now
        filename = outputDir </> basename <.> "md"
        epub     = outputDir </> basename <.> "epub"
    md <- liftIO $ metadata ps
    liftIO
        . TLIO.writeFile filename
        . toLazyText
        . mconcat
        . (md:)
        $ map renderChapter bs
    printEpub3 filename epub

metadata :: M.HashMap Int64 (Entity Profile) -> IO Builder
metadata ps = do
    now <-  formatTime defaultTimeLocale (iso8601DateFormat Nothing)
        <$> getCurrentTime
    return $ build "---\n\
                \title: Dialogues\n\
                \creator:\n\
                \- type: editor\n\
                \  text: Eric\n\
                \{}\
                \date: {}\n\
                \description: Traces of a conversation.\n\
                \---\n\
                \\n"
                (authors, now)
    where
        authors = foldMap ( build "- type: author\n  text: {}\n"
                          . Only
                          . _profileNickname
                          . entityVal
                          )
                $ M.elems ps

renderChapter :: [PublishBlock] -> Builder
renderChapter ps@(p:_) =
    mappend h1 . foldMap renderGroup $ groupBlocks blockSpan ps
    where
        h1 = fromString $ formatTime defaultTimeLocale "# %B %Y\n\n" $ _pbDate p
renderChapter [] = mempty

groupBlocks :: NominalDiffTime -> [PublishBlock] -> [BlockGroup]
groupBlocks dt (pb:pbs) =
    toList . uncurry (flip (|>)) $ foldl' step (single pb, mempty) pbs
    where
        single p = BlockGroup (_pbDate p) (_pbDate p) $ Seq.singleton p

        step p@(current, accum) b
            | diffTime > dt = (single b, accum |> current)
            | otherwise = p & _1 . bgEndTime .~ _pbDate b
                            & _1 . bgBlocks  %~ (|> b)
            where
                diffTime = _bgEndTime current `diffUTCTime` _pbDate b

groupBlocks _ [] = []

renderGroup :: BlockGroup -> Builder
renderGroup (Seq.viewl . _bgBlocks -> b :< bs) =
        renderBlock True b <> foldMap (renderBlock False) bs
renderGroup _ = mempty

renderBlock :: Bool -> PublishBlock -> Builder
renderBlock showTime PublishBlock{..} =
    mconcat [ build "<div class='{}'>\n\n" $ Only pClass
            , build "## {} <span class='service {}'>{}</span>\n\n"
                    (_pbName, serviceClass _pbService, serviceIcon _pbService)
            , if showTime
                then fromString $ formatTime defaultTimeLocale
                                             "### %A, %e %B %Y, %H:%M\n\n"
                                             _pbDate
                else mempty
            , fromText _pbContent
            , "\n\n</div>\n\n"
            ]
    where
        pClass :: T.Text
        pClass = if _pbPrimary then "primary" else "secondary"

serviceIcon :: Service -> Char
serviceIcon AdiumService   = '\xf086'
serviceIcon GoogleService  = '\xf0d5'
serviceIcon JournalService = '\xf02d'
serviceIcon NoteService    = '\xf040'
serviceIcon TwitterService = '\xf099'

serviceClass :: Service -> T.Text
serviceClass AdiumService   = "adium"
serviceClass GoogleService  = "google"
serviceClass JournalService = "journal"
serviceClass NoteService    = "note"
serviceClass TwitterService = "twitter"

printEpub3 :: FilePath -> FilePath -> Dialogue ()
printEpub3 mdFile epubFile = do
    style <- liftIO $ readFile =<< getDataFileName "epub-files/style.css"
    font  <- liftIO $ getDataFileName "epub-files/FontAwesome.otf"
    let ropts = def { readerSmart      = True
                    , readerExtensions = pandocExtensions
                    }
        wopts = def { writerStandalone      = True
                    , writerEpubVersion     = Just EPUB3
                    , writerEpubStylesheet  = Just style
                    , writerEpubFonts       = [font]
                    , writerTOCDepth        = 1
                    }
    doc <-  hoistE . first toException . readMarkdown ropts
        =<< liftIO (readFile mdFile)
    liftIO $ BL.writeFile epubFile =<< writeEPUB wopts doc
