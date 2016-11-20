module Main where

import System.Environment

import Network.Wreq
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import Data.Char
import Data.List

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Text.Regex.TDFA ((=~))
import Text.Regex

import NLP.POS
import NLP.Tokenize.String

type Url = String

data Page = Page { topic::String, text::[String]} deriving (Show)

replaceReg :: String -> String -> String -> String
replaceReg regex replacement = flip (subRegex (mkRegex regex)) replacement

randomUrl :: Url
randomUrl = "https://en.wikipedia.org/wiki/Special:Random"

makeWikipediaUrl :: String -> Url
makeWikipediaUrl = ( "https://en.wikipedia.org/wiki/" ++ )

fetchPageCandidate :: Url -> IO Page
fetchPageCandidate url = do
  r <- get url
  let doc = readString [withParseHTML yes, withWarnings no] . C8.unpack . BL.toStrict $ r ^. responseBody
  topics <- runX $ doc >>> css "h1" /> getText
  paragraphs <- runX $ doc //> hasAttrValue "id" (== "mw-content-text") /> hasName "p" //> getText
  if (url == randomUrl && length topics == 0 )
  then do
    putStrLn "No H1, fetch another"
    fetchPageCandidate url
  else
    return (Page (head topics) paragraphs)

fetchPage :: Url -> IO Page
fetchPage url = do
  putStrLn "Fetching page..."
  page <- fetchPageCandidate url
  if (url == randomUrl && (
       -- skip lists
       ( "List " `isPrefixOf` (topic page) || "Lists " `isPrefixOf` (topic page) ) ||
       -- skip disambiguation pages
       length (text page) == 0 || (head (text page) =~ "[can|may] refer to" )
     ))
  then do
    putStrLn "was a page list, fetch another"
    (fetchPage url)
  else (return $ cleanSentences page)

cleanSentences :: Page -> Page
cleanSentences (Page topic paragraphs) =
  let content = concat $ intersperse " " paragraphs
      cleanedContent =
        replaceReg "\\[[0-9].?\\]" "" $
        replaceReg "[[:space:]]+" " " $
        replaceReg "\\(.*\\)" "" content
      sentences = splitRegex (mkRegex "\\. ") cleanedContent
   in Page topic sentences

-- tagPage :: PosTagger Tag -> Page -> [[String]]
tagPage tagger page = filter hasVerb taggedSentences
  where
    taggedSentences = fmap tagSentence (text page)

    tagSentence :: String -> [String]
    tagSentence s = fmap (tagStr tagger) (tokenize s)

    hasVerb :: [String] -> Bool
    hasVerb = any ("/VB" `isInfixOf`)

mergeSentences :: ([String],[String]) -> String
mergeSentences (primary, secondary) = tagsToSentence $ (fst $ splitVerb primary) ++ (snd $ splitVerb secondary)
  where
    splitVerb :: [String] -> ([String], [String])
    splitVerb tags = (untilVerb, afterVerb) where
      (begin, end) = break ("/VB" `isInfixOf`) tags
      untilVerb = begin ++ [head end]
      afterVerb = tail end

    tagsToSentence :: [String] -> String
    tagsToSentence tags = cleanSentence . concat . (intersperse " ") $ fmap tagToWord tags

    tagToWord :: String -> String
    tagToWord tag = reverse word where
      '/':word = snd $ span ( /= '/') (reverse tag)

    cleanSentence :: String -> String
    cleanSentence =
      replaceReg " \\." "." .
      replaceReg " \\," "."

main :: IO ()
main = do
  args <- getArgs
  [page1, page2] <- mapM fetchPage $ parseArgs args
  tagger <- defaultTagger
  let facts = fmap mergeSentences $ zip (tagPage tagger page1) (tagPage tagger page2)
  putStrLn "\n================================="
  putStrLn $ topic page1 ++ " + " ++ topic page2
  putStrLn "=================================\n"
  mapM_ putStrLn facts

parseArgs :: [String] -> [Url]
parseArgs (arg1:(arg2:_)) = [makeWikipediaUrl arg1, makeWikipediaUrl arg2]
parseArgs [arg] = [makeWikipediaUrl arg, randomUrl]
parseArgs [] = [randomUrl, randomUrl]

