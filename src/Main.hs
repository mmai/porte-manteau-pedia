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
import Text.HTML.TagSoup
-- import Text.HTML.TagSoup hiding (parseTags, renderTags)
-- import Text.HTML.TagSoup.Fast


type Url = String

-- data Page = Page { topic::ByteString, text:: [ByteString]} deriving (Show)

randomUrl :: Url
randomUrl = "https://en.wikipedia.org/wiki/Special:Random"

makeWikipediaUrl :: String -> Url
makeWikipediaUrl = ( "https://en.wikipedia.org/wiki/" ++ )

fetchPage :: Url -> IO String
-- fetchPage :: Url -> IO B.ByteString
-- fetchPage :: Url -> IO Page
fetchPage url = do
  r <- get url
  let src = C8.unpack . BL.toStrict $ r ^. responseBody
  let pageTopic = fromH1 $ parseTags src
  return pageTopic
  -- return $ Page "pageTopic" []

-- fromH1 :: [Tag B.ByteString] -> B.ByteString
fromH1 :: [Tag String] -> String
fromH1 =  innerText . take 2 . dropWhile (~/= "<h1>")

combinePages :: [String] -> String
-- combinePages :: [B.ByteString] -> B.ByteString
combinePages [first, second] = concat [ first, "\n\n===========\n\n",  second ]
-- combinePages :: [Page] -> BL.ByteString
-- combinePages [first, second] = BL.concat [topic first, "\n\n===========\n\n", topic second ]

main :: IO ()
main = do
  args <- getArgs
  pages <- mapM fetchPage $ parseArgs args
  print $ combinePages pages

parseArgs :: [String] -> [Url]
parseArgs (arg1:(arg2:_)) = [makeWikipediaUrl arg1, makeWikipediaUrl arg2]
parseArgs [arg] = [makeWikipediaUrl arg, randomUrl]
parseArgs [] = [randomUrl, randomUrl]

