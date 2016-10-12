{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}


module Dialogue.Actions.Publish where


import           Control.Arrow           hiding (first)
import           Control.Error
import           Control.Lens            hiding ((:<), (<.>), (|>))
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Function           (on)
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
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
import           System.Process

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
        single p = let pbd = _pbDate p
                   in  BlockGroup pbd pbd $ Seq.singleton p

        step p@(current, accum) b
            | (diffTime >= dt) || (diffTime < -dt) =
                            (single b, accum |> current)
            | otherwise = p & _1 . bgEndTime .~ _pbDate b
                            & _1 . bgBlocks  %~ (|> b)
            where
                diffTime = _bgEndTime current `diffUTCTime` _pbDate b

groupBlocks _ [] = []

renderGroup :: BlockGroup -> Builder
renderGroup (Seq.viewl . _bgBlocks -> b :< bs) =
        renderBlock True b <> foldMap (renderBlock False) bs <> "---\n\n"
renderGroup _ = mempty

renderBlock :: Bool -> PublishBlock -> Builder
renderBlock showTime pb@PublishBlock{..} =
    mconcat [ build "<div class='{}'>\n\n" $ Only pClass
            , build "## <span class='service {}'>{}</span> {}\n\n"
                    (renderClass pb, _pbName, renderIcon pb)
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

renderIcon :: PublishBlock -> Builder
renderIcon PublishBlock{_pbService=AdiumService}   = singleton '\xf086'
renderIcon PublishBlock{_pbService=JournalService} = singleton '\xf02d'
renderIcon PublishBlock{_pbService=NoteService}    = singleton '\xf040'
renderIcon PublishBlock{_pbService=TwitterService} = singleton '\xf099'
renderIcon PublishBlock{_pbService=GoogleService, _pbTags=tags}
    | "SMS"  `S.member` tags = singleton '\xf10b'
    | "CHAT" `S.member` tags = singleton '\xf0c0'
    | otherwise              = singleton '\xf0d5'

renderClass :: PublishBlock -> T.Text
renderClass PublishBlock{_pbService=AdiumService}   = "adium"
renderClass PublishBlock{_pbService=JournalService} = "journal"
renderClass PublishBlock{_pbService=NoteService}    = "note"
renderClass PublishBlock{_pbService=TwitterService} = "twitter"
renderClass PublishBlock{_pbService=GoogleService, _pbTags=tags}
    | "SMS"  `S.member` tags = "google sms"
    | "CHAT" `S.member` tags = "google chat"
    | otherwise              = "google"

printEpub3 :: FilePath -> FilePath -> Dialogue ()
printEpub3 mdFile epubFile = do
    style <- liftIO $ getDataFileName "epub-files/style.css"
    font  <- liftIO $ getDataFileName "epub-files/FontAwesome.otf"
    liftIO $ callProcess "pandoc" [ "--smart"
                                  , "--epub-stylesheet", style
                                  , "--epub-embed-font", font
                                  , "--epub-chapter-level", "1"
                                  , "--from", "markdown+autolink_bare_uris"
                                  , "--to", "epub3"
                                  , "--output", epubFile
                                  , mdFile
                                  ]
