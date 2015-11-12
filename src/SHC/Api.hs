{-# LANGUAGE OverloadedStrings #-}
module SHC.Api (sendData, readCoverageResult)
    where

import Safe (atMay, headMay)
import Data.Aeson (Value, encode)
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Codec.Binary.UTF8.String (decode)
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

-- | Extract the total coverage percentage value from coveralls coverage result
--   page content.  The current implementation is kept as low level as possible
--   in order not to increase the library build time, by not relying on
--   additional packages.
extractCoverage :: T.Text -> Maybe T.Text
extractCoverage body = T.splitOn "<"
                    <$> T.splitOn prefix body `atMay` 1 >>= headMay
    where prefix = "div class='run-statistics'>\n<strong>"

-- | Read the coveraege result page from coveralls.io
readCoverageResult :: String -> IO (Maybe String)
readCoverageResult url = do
    r <- get url
    return . fmap T.unpack . extractCoverage $
        r ^. responseBody . _String
