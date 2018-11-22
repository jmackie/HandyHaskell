{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http (statusCode)
import Control.Exception (try, throwIO)
import Data.ByteString.Lazy (ByteString)


pattern BadStatus :: Int -> Http.HttpException
pattern BadStatus n <-
    Http.HttpExceptionRequest _
        (Http.StatusCodeException (responseStatusCode -> n) _)


pattern Status404 :: Http.HttpException
pattern Status404 <- BadStatus 404


responseStatusCode :: Http.Response a -> Int
responseStatusCode = Http.statusCode . Http.responseStatus


-- | Make a request and return `Nothing` if the response is 404.
getMaybe
    :: Http.Manager
    -> Http.Request
    -> IO (Maybe (Http.Response ByteString))
getMaybe manager request = do
    result <- try (Http.httpLbs request manager)
    case result of
        Right response  -> pure (Just response)
        Left Status404  -> pure Nothing
        Left  err       -> throwIO err


main :: IO ()
main = do
    manager  <- Http.newManager Http.defaultManagerSettings

    -- NOTE: parseUrlThrow throws an exception for non-2XX response codes
    let mkRequest = Http.parseUrlThrow

    -- e.g. Just
    mkRequest "http://httpbin.org/status/200" >>= getMaybe manager >>= print
    -- e.g. Nothing
    mkRequest "http://httpbin.org/status/404" >>= getMaybe manager >>= print
