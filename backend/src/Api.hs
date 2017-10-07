{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Database.SQLite.Simple     as Sql
import           Elm                        (Spec (Spec), specsToDir,
                                             toElmDecoderSource,
                                             toElmTypeSource)
import           GHC.Generics               (Generic)
import qualified Model                      as M
import           Servant
import           Servant.API                ((:>), Capture, Get, JSON)
import           Servant.Elm                (ElmType, Proxy (Proxy),
                                             defElmImports, generateElmForAPI)
import qualified Storage                    as S

instance ToJSON M.Artist
instance FromJSON M.Artist
instance ToJSON M.Track
instance FromJSON M.Track
instance ToJSON M.Album
instance FromJSON M.Album
instance ElmType M.Artist
instance ElmType M.Album
instance ElmType M.Track


type ArtistAPI =
       Get '[JSON] [M.Artist]
  :<|> Capture "artistId" Int :> Get '[JSON] M.Artist
  :<|> ReqBody '[JSON] M.Artist :> Post '[JSON] M.Artist

artistsServer :: Sql.Connection -> Server ArtistAPI
artistsServer conn = getArtists conn :<|> getArtist conn :<|> postArtist conn

getArtists :: Sql.Connection -> Handler [M.Artist]
getArtists conn = liftIO $ S.findArtists conn

postArtist :: Sql.Connection -> M.Artist -> Handler M.Artist
postArtist conn artist = liftIO $ S.newArtist conn artist

getArtist :: Sql.Connection -> Int -> Handler M.Artist
getArtist conn artistId = do
  artist <- liftIO $ S.artistById conn artistId
  case artist of
    Nothing -> throwError err404
    Just a  -> return a

type AlbumAPI = Get '[JSON] [M.Album]

albumsServer :: Sql.Connection -> Server AlbumAPI
albumsServer conn = getAlbums conn

getAlbums :: Sql.Connection -> Handler [M.Album]
getAlbums conn = liftIO $ S.findAlbums conn

liftIOMaybeToEither :: (MonadIO m) => a -> IO (Maybe b) -> EitherT a m b
liftIOMaybeToEither err x = do
    m <- liftIO x
    case m of
      Nothing -> left err
      Just x  -> right x


type API = "artists" :> ArtistAPI :<|> "albums" :> AlbumAPI

combinedServer :: Sql.Connection -> Server API
combinedServer conn = artistsServer conn :<|> albumsServer conn

api :: Proxy API
api = Proxy


-- (Handler a) equivalent to a computation of type (IO (Either ServantErr a))
