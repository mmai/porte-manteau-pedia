-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment

import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import qualified Data.ByteString as B
import Data.Char
import Data.List
import Text.HTML.TagSoup
-- import Text.HTML.TagSoup hiding (parseTags, renderTags)
-- import Text.HTML.TagSoup.Fast


type Url = String

data Page = Page { topic::String, text:: [String]} deriving (Show)
-- data Page = Page { topic::ByteString, text:: [ByteString]} deriving (Show)

randomUrl :: Url
randomUrl = "https://en.wikipedia.org/wiki/Special:Random"

makeWikipediaUrl :: String -> Url
makeWikipediaUrl = ( "https://en.wikipedia.org/wiki/" ++ )

fetchPageCandidate :: Url -> IO Page
fetchPageCandidate url = do
  r <- get url
  let tags = parseTags . C8.unpack . BL.toStrict $ r ^. responseBody
  let pageTopic = fromH1 tags
  -- let pageContent = fromParagraphs tags
  return $ Page pageTopic []

fetchPage :: Url -> IO Page
fetchPage url = do
  putStrLn "Fetching page..."
  page <- fetchPageCandidate url
  -- skip lists
  if (url == randomUrl && (
    "List " `isPrefixOf` (topic page) ||
    "Lists " `isPrefixOf` (topic page)
    ))
  then do
    putStrLn "was a page list, fetch another"
    (fetchPage url)
  else (return page)

fromH1 :: [Tag String] -> String
fromH1 =  innerText . take 2 . dropWhile (~/= "<h1>")

-- fromParagraphs :: [Tag String] -> [String]
-- fromParagraphs = []

combinePages :: [Page] -> String
combinePages [first, second] = concat ["\n", topic first, "n\n===========\n\n", topic second, "\n"]

main :: IO ()
main = do
  args <- getArgs
  pages <- mapM fetchPage $ parseArgs args
  putStr $ combinePages pages

parseArgs :: [String] -> [Url]
parseArgs (arg1:(arg2:_)) = [makeWikipediaUrl arg1, makeWikipediaUrl arg2]
parseArgs [arg] = [makeWikipediaUrl arg, randomUrl]
parseArgs [] = [randomUrl, randomUrl]

