{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding

import Network.Wreq
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BL

import Text.XML.HXT.Core
import Text.HandsomeSoup

import Data.Attoparsec.Text as Atto

import NLP.POS
import NLP.Types
import NLP.Tokenize.Text

type Url = String

data Page = Page { topic::T.Text, text::[T.Text]} deriving (Show)

randomUrl :: Url
randomUrl = "https://fr.wikipedia.org/wiki/Spécial:Page_au_hasard"

makeWikipediaUrl :: String -> Url
makeWikipediaUrl = ( "https://fr.wikipedia.org/wiki/" ++ )

fetchPageCandidate :: Url -> IO Page
fetchPageCandidate url = do
  r <- get url
  let doc = readString [withParseHTML yes, withWarnings no, withInputEncoding utf8] . T.unpack . decodeUtf8 . BL.toStrict $ r ^. responseBody
  topics <- runX $ doc >>> css "h1" /> getText
  paragraphs <- runX $ doc //> hasAttrValue "id" (== "mw-content-text") /> hasName "p" //> getText
  if (url == randomUrl && length topics == 0 )
  then do
    putStrLn "No H1, fetch another"
    fetchPageCandidate url
  else
    return $ Page (T.pack $ head topics) (fmap T.pack paragraphs)

fetchPage :: Url -> IO Page
fetchPage url = do
  putStrLn "Fetching page..."
  page <- fetchPageCandidate url
  if (url == randomUrl && (
       -- skip lists
       ( "List " `T.isPrefixOf` (topic page) || "Lists " `T.isPrefixOf` (topic page) ) ||
       -- skip disambiguation pages
       -- length (text page) == 0 || (head (text page) =~ "[can|may] refer to" )
       length (text page) == 0
     ))
  then do
    putStrLn "was a page list, fetch another"
    (fetchPage url)
  else do
    -- putStrLn . T.unpack . T.concat $ text page
    (return $ cleanSentences page)

cleanSentences :: Page -> Page
cleanSentences (Page topic paragraphs) =
  let content =  T.unwords $  T.words $  T.intercalate " " paragraphs
      Right cleanedContent = parseOnly removeParBrac content
      Right sentences = parseOnly (splitBy '.') cleanedContent
   in Page topic sentences

splitBy :: Char -> Parser [T.Text]
splitBy c = Atto.takeWhile (/= c) `sepBy` (char c)

removeParBrac :: Parser T.Text
removeParBrac = fmap T.concat $ manyTill removeOnePar endOfInput

removeOnePar = do
  skipMany $ delimitedParser '(' ')'
  skipMany $ delimitedParser '[' ']'
  Atto.takeWhile (\c -> c /= '(' && c /= '[')

delimitedParser :: Char -> Char -> Parser T.Text
delimitedParser begin end = char begin *> Atto.takeWhile (/= end) *> char end *> return T.empty

tagPage :: POSTagger RawTag -> Page -> [[T.Text]]
tagPage tagger page = filter hasVerb taggedSentences
  where
    taggedSentences = fmap tagSentence (text page)

    tagSentence :: T.Text -> [T.Text]
    tagSentence s = fmap (tagText tagger) (tokenize s)

    hasVerb :: [T.Text] -> Bool
    hasVerb = any ("/V" `T.isInfixOf`)

mergeSentences :: ([T.Text],[T.Text]) -> T.Text
mergeSentences (primary, secondary) = tagsToSentence $ (fst $ splitVerb primary) ++ (snd $ splitVerb secondary)
  where
    splitVerb :: [T.Text] -> ([T.Text], [T.Text])
    splitVerb tags = (untilVerb, afterVerb) where
      (begin, end) = break ("/V" `T.isInfixOf`) tags
      untilVerb = begin ++ [head end]
      afterVerb = tail end

    tagsToSentence :: [T.Text] -> T.Text
    tagsToSentence tags = ( T.intercalate " ") $ fmap tagToWord tags

    tagToWord :: T.Text -> T.Text
    tagToWord = (T.dropEnd 1) . (T.dropWhileEnd (/= '/'))

main :: IO ()
main = do
  args <- getArgs
  [page1, page2] <- mapM fetchPage $ parseArgs args
  -- tagger <- defaultTagger
  tagger <- (loadTagger "src/head1000.model":: IO (POSTagger RawTag))
  let facts = fmap mergeSentences $ zip (tagPage tagger page1) (tagPage tagger page2)
  putStrLn "\n================================="
  putStrLn . T.unpack $ T.concat [topic page1, " + ", topic page2]
  putStrLn "=================================\n"
  mapM_ (putStrLn . T.unpack) facts

parseArgs :: [String] -> [Url]
parseArgs (arg1:(arg2:_)) = [makeWikipediaUrl arg1, makeWikipediaUrl arg2]
parseArgs [arg] = [makeWikipediaUrl arg, randomUrl]
parseArgs [] = [randomUrl, randomUrl]

-- tests
str = T.pack "un (deux) trois [quatre] cinq"
