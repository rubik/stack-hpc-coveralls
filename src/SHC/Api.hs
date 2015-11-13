{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      SHC.Lix
-- Copyright:   (c) 2015 Michele Lacchia
-- License:     ISC
-- Maintainer:  Michele Lacchia <michelelacchia@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for sending data to Coveralls.io and reading results.

module SHC.Api (sendData, readCoverageResult)
    where

import Data.Aeson (Value, encode)
import Data.Aeson.Lens (key, _Double, _String)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Codec.Binary.UTF8.String (decode)
import Control.Applicative ((<$>))
import Control.Lens
import Network.Wreq
import Network.HTTP.Client (RequestBody(RequestBodyLBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)

import SHC.Types


sendData :: Config -> String -> Value -> IO PostResult
sendData conf url json = do
    r <- post url [partFileRequestBody "json_file" fileName requestBody]
    if r ^. responseStatus . statusCode == 200
       then return $ readResponse r
       else return . PostFailure $
           "Error: " ++ decode (BS.unpack $ r ^. responseStatus . statusMessage)
    where fileName    = serviceName conf ++ "-" ++ jobId conf ++ ".json"
          requestBody = RequestBodyLBS $ encode json

readResponse :: Response LBS.ByteString -> PostResult
readResponse r =
    case r ^? responseBody . key "error" . _String of
      Just err -> PostFailure $ T.unpack err
      Nothing  -> case r ^? responseBody . key "url" . _String of
                    Just url -> PostSuccess $ T.unpack url
                    Nothing  -> PostFailure "Error: malformed response body"

-- | Read the coveraege result page from coveralls.io
readCoverageResult :: String -> IO (Maybe Double)
readCoverageResult url = do
    r <- get url
    return $ r ^? responseBody . key "covered_percent" . _Double
