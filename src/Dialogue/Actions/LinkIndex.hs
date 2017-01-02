{-# LANGUAGE OverloadedLists #-}


module Dialogue.Actions.LinkIndex where


import           Control.Error
import qualified Data.List         as L
import qualified Data.Set          as S
import           Text.Pandoc
import           Text.Pandoc.Error


extractURL :: Inline -> [String]
extractURL (Link  _ _ (u, _)) = [u]
extractURL (Image _ _ (u, _)) = [u]
extractURL _                  = []

extractURLs :: Pandoc -> [String]
extractURLs = queryWith extractURL

isMailTo :: String -> Bool
isMailTo u | "mailto:" `L.isPrefixOf` u = True
           | otherwise                  = False

formatUrls :: [String] -> String
formatUrls = unlines
           . ("# Links":)
           . ("":)
           . map (('*':) . (' ':))
           . S.toAscList
           . S.filter (not . isMailTo)
           . S.fromList

readDoc :: String -> Either PandocError Pandoc
readDoc =
    readMarkdown def { readerSmart      = True
                     , readerStandalone = True
                     , readerExtensions =
                            S.insert Ext_autolink_bare_uris pandocExtensions
                     }

indexLinks :: Script ()
indexLinks =
    scriptIO $ interact (either show (formatUrls . extractURLs) . readDoc)
