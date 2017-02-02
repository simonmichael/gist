#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package aeson
  --package bytestring
  --package conduit
  --package http-conduit
  --package resourcet
  --package text
-}

{-
gist.hs - a command-line client for gist.github.com that works.
(c) 2012-2017 Simon Michael <simon@joyful.com>
-}

{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}

import Control.Applicative ((<$>), empty)
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit
import qualified Data.Text as T
import Network.HTTP.Conduit
import System.IO
import System.Environment

version = "0.1.98"

data Paste = Paste {
  pasteDescription :: T.Text,
  pastePublic :: Bool,
  pasteFiles :: Value
}

instance ToJSON Paste where
    toJSON Paste{..} = object [
                               "description" .= pasteDescription
                              ,"public" .= pastePublic
                              ,"files" .= pasteFiles
                              ]

data PasteResponse = PasteResponse {
  pasteResponseUrl :: String
}

instance FromJSON PasteResponse where
  parseJSON (Object v) = PasteResponse <$> v .: "html_url"
  parseJSON _ = empty

main = do
  args <- getArgs
  hasstdin <- hReady stdin
  case (args, hasstdin) of
    ([f],_)   -> readFile f >>= paste "" (T.pack f) >>= report
    ([],True) -> getContents >>= paste "" "" >>= report
    _         -> usage

usage = putStr $ unlines
  ["gist "++version
  ,"Usage: gist FILE or cat FILE | gist to make a public paste at gist.github.com"
  ]

paste description filename content = do
  let paste = Paste description True $ object [filename .= object ["content" .= content]]
  req <- parseUrl "https://api.github.com/gists"
  runResourceT $ do
    withManager $ \mgr -> do
      httpLbs req{method = "POST", requestBody = RequestBodyLBS $ encode paste} mgr

report rsp = do
  putStrLn $ show $ responseStatus rsp
  let
    body = responseBody rsp
    mresponse = decode body
  putStrLn $ maybe ("Unknown response:\n" ++ (BL.unpack body)) pasteResponseUrl mresponse
