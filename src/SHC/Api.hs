{-# LANGUAGE CPP               #-}
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

import           Codec.Binary.UTF8.String              (decode)
import           Control.Exception                     (catch)
import           Data.Aeson                            (Value, encode)
import           Data.Aeson.Lens                       (key, _Double, _String)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text                             as T
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                   ((<$>))
#endif
import           Control.Lens
import           Network.HTTP.Client                   (RequestBody (RequestBodyLBS))
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Network.Wreq

import           SHC.Types                             (Config (..),
                                                        PostResult (..))


-- | Send coverage JSON to Coveralls.io.
sendData :: Config        -- ^ SHC configuration
         -> String        -- ^ URL
         -> Value         -- ^ The JSON object
         -> IO PostResult
sendData conf url json = do
    r <- postWith httpOptions url [partFileRequestBody "json_file" fileName requestBody]
    if r ^. responseStatus . statusCode == 200
       then return $ readResponse r
       else return . PostFailure $ formatResponseError r
    where fileName    = serviceName conf ++ "-" ++ jobId conf ++ ".json"
          requestBody = RequestBodyLBS $ encode json
          httpOptions = defaults & checkStatus .~ Just noCheck
          noCheck _ _ _ = Nothing

readResponse :: Response LBS.ByteString -> PostResult
readResponse r =
    case r ^? responseBody . key "error" . _String of
      Just _  -> PostFailure $ formatResponseError r
      Nothing -> case r ^? responseBody . key "url" . _String of
                   Just url -> PostSuccess $ T.unpack url
                   Nothing  -> PostFailure "Error: malformed response body"

formatResponseError :: Response LBS.ByteString -> String
formatResponseError r =
    "Coveralls returned HTTP " ++ show (r ^. responseStatus . statusCode) ++
    " " ++ decode (BS.unpack $ r ^. responseStatus . statusMessage) ++ "\n" ++
    decode (LBS.unpack $ r ^. responseBody)

-- | Read the coverage results from Coveralls.io.
readCoverageResult :: String -> IO (Maybe Double)
readCoverageResult url = do
    r <- get url
    return $ r ^? responseBody . key "covered_percent" . _Double
