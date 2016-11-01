module Slidecoding.SlidesWriter
    ( processSlides
    ) where

import Slidecoding.Template
import Slidecoding.Types

import Prelude                           hiding (lookup)

import Codec.Binary.Base64.String as B64 (decode)
import Data.List                         (find, isPrefixOf)
import Data.Map                          (lookup)
import Data.Maybe                        (fromMaybe)
import Data.Monoid                       ((<>))
import System.FilePath                   ((</>), dropExtension,takeFileName)
import Text.Pandoc                       (Pandoc(..), Meta(..), Block(..), Inline(..), def, nullAttr
                                        , nullMeta, readMarkdown, writePlain, writeHtml)
import Text.Pandoc.Definition            (MetaValue(..))

processSlides :: Presentation -> [Description] -> [FilePath] -> IO ()
processSlides presentation descs chapters = do
  distributeAssets presentation
  document <- joinSections presentation <$> mapM (eachChapter descs) chapters
  writeFile outputFile document
    where outputFile = distDir presentation </> "index.html"

eachChapter :: [Description] -> FilePath -> IO (FilePath, Pandoc)
eachChapter descs file = pipeline <$> readFile file
  where pipeline = (,) file . walkSlides descs . readChapter file

joinSections :: Presentation -> [(FilePath, Pandoc)] -> String
joinSections presentation slides = renderHtml (template design' title' document)
  where document   = titleSlide <> content
        content    = mconcatWith writeSection slides
        meta'      = meta presentation
        titleSlide = writeSection (titleSection presentation)
        title'     = title meta'
        design'    = design meta'

titleSection :: Presentation -> (FilePath, Pandoc)
titleSection presentation = (file, doc)
  where file    = rootDir presentation </> "presentation.yaml"
        doc     = Pandoc nullMeta [block]
        block   = Header 1 ("", classes, []) [inline]
        classes = ["presentation-title"]
        inline  = Str title'
        meta'   = meta presentation
        title'  = title meta'

mconcatWith :: (Monoid b) => (a -> b) -> [a] -> b
mconcatWith m = mconcat . fmap m

data Chapter = Chapter FilePath Pandoc
newtype Section = Section [Block]

readChapter :: FilePath -> String -> Chapter
readChapter f s = either handleFailure buildChapter parse
  where parse         = readMarkdown def s
        buildChapter  = Chapter f
        handleFailure = error . show

offsetHeaders :: Int -> Block -> Block
offsetHeaders off (Header n attrs blocks) = Header (n + off) attrs blocks
offsetHeaders _ b = b

writeSection :: (FilePath, Pandoc) -> Html
writeSection (file, Pandoc _ blocks) = asComment file <> mconcatWith writeSection' sections
  where sections = groupBySection blocks

writeSection' :: Section -> Html
writeSection' (Section (Header n (id', classes, properties) titleContent : blocks)) =
  section $ writeHtml def section'
    where section' = Pandoc nullMeta (Header n ("", [], properties) titleContent : blocks)
          section = mkSection id' classes
writeSection' (Section content) =
  wrapSection $ writeHtml def section'
    where section' = Pandoc nullMeta content

groupBySection :: [Block] -> [Section]
groupBySection [] = []
groupBySection (b : bs) | isTitle b = Section (b : sectionContent) : groupBySection rest
  where (sectionContent, rest) = break isTitle bs
groupBySection bs = Section sectionContent : groupBySection rest
  where (sectionContent, rest) = break isTitle bs

isTitle :: Block -> Bool
isTitle (Header 1 _ _) = True
isTitle (Header 2 _ _) = True
isTitle  _             = False

walkSlides :: [Description] -> Chapter -> Pandoc
walkSlides descs (Chapter file doc) = Pandoc m content
  where Pandoc m bs = doc
        content     = titleSlide : slides
        titleSlide  = chapterTitle key title'
        slides      = map (offsetHeaders 1) $ concatMap (replaceSourceBlock descs) bs
        key         = dropExtension.takeFileName $ file
        title'      = fromMaybe key (readTitle m)

readTitle :: Meta -> Maybe String
readTitle (Meta attrs) = lookup "title" attrs >>= expectString
  where expectString (MetaInlines is) = Just (writePlain def (Pandoc nullMeta [Plain is]))
        expectString (MetaString   s) = Just s
        expectString  _               = Nothing

chapterTitle :: String -> String -> Block
chapterTitle key title' = Header 1 attributes [Str title']
  where attributes = (key, ["chapter-title"], [])

replaceSourceBlock :: [Description] -> Block -> [Block]
replaceSourceBlock descs original@(Para [Link _ contents (url, _)]) | isSourceUrl url = expandLink (inlineLink descs url)
  where expandLink (Just b) = [Para contents, b]
        expandLink Nothing  = [original]
replaceSourceBlock _ b = [b]

isSourceUrl :: String -> Bool
isSourceUrl url = "source://" `isPrefixOf` url

inlineLink :: [Description] -> String -> Maybe Block
inlineLink descs url = CodeBlock nullAttr <$> source
  where source = loadSource descs url

loadSource :: [Description] -> String -> Maybe String
loadSource descs url = parseUrl url >>= lookupSource descs

parseUrl :: String -> Maybe (ModuleName, String)
parseUrl url = toPair tokens
  where tokens        = splitOn '/' suffix
        suffix        = drop (length "source://") url
        toPair [m, f] = Just (m, f)
        toPair _      = Nothing

splitOn :: Char -> String -> [String]
splitOn c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitOn c s''
      where (w, s'') = break (== c) s'

lookupSource :: [Description] -> (ModuleName, String) -> Maybe String
lookupSource descs (m, f) = do
  Description _ items   <- find (ofModule m) descs
  Item _ _ (Source b64) <- find (ofFunction f) items
  return (B64.decode b64)

ofModule :: ModuleName -> Description -> Bool
ofModule m (Description (Module _ mn) _) = m == mn

ofFunction :: String -> Item -> Bool
ofFunction f (Item (Symbol s) _ _) = f == s
