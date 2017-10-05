{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Api                         as A
import qualified Bootstrap                   as B
import           Control.Exception           (bracket)
import           Database.SQLite.Simple      as Sql
import           Elm                         (ElmType)
import           Elm                         (Spec (Spec), specsToDir,
                                              toElmDecoderSource,
                                              toElmEncoderSource,
                                              toElmTypeSource)
import           Model                       as M
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant


app :: Sql.Connection -> Application
app conn = serve A.api (A.combinedServer conn)


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
  withConn $ \conn -> cb conn
  where
    withConn = bracket testConnect Sql.close


albumCors :: Middleware
albumCors = cors $ const (Just albumResourcePolicy)


albumResourcePolicy :: CorsResourcePolicy
albumResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }


specs :: [Spec]
specs =
  [ Spec
    ["generated", "api"]
     [ "import Json.Decode exposing (..)"
     , "import Json.Decode.Pipeline exposing (..)"
     , toElmTypeSource    (Proxy :: Proxy M.Artist)
     , toElmDecoderSource (Proxy :: Proxy M.Artist)
     , toElmEncoderSource (Proxy :: Proxy M.Artist)
     , toElmTypeSource    (Proxy :: Proxy M.Album)
     , toElmDecoderSource (Proxy :: Proxy M.Album)
     , toElmEncoderSource (Proxy :: Proxy M.Album)
     , toElmTypeSource    (Proxy :: Proxy M.Track)
     , toElmDecoderSource (Proxy :: Proxy M.Track)
     , toElmEncoderSource (Proxy :: Proxy M.Track)

     ]
  ]




main :: IO ()
main = do
  specsToDir specs "frontend/src"
  withTestConnection $ \conn ->  do
    B.bootstrapDB conn
    run 8081 $ albumCors $ app conn
