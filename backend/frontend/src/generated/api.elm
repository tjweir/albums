module generated.api exposing (..)

import Json.Decode exposing (..)

import Json.Decode.Pipeline exposing (..)

type alias Artist =
    { artistId : Maybe (Int)
    , artistName : String
    }

decodeArtist : Decoder Artist
decodeArtist =
    decode Artist
        |> required "artistId" (maybe int)
        |> required "artistName" string

encodeArtist : Artist -> Json.Encode.Value
encodeArtist x =
    Json.Encode.object
        [ ( "artistId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.artistId )
        , ( "artistName", Json.Encode.string x.artistName )
        ]

type alias Album =
    { albumId : Maybe (Int)
    , albumName : String
    , albumArtistId : Int
    , albumTracks : List (Track)
    }

decodeAlbum : Decoder Album
decodeAlbum =
    decode Album
        |> required "albumId" (maybe int)
        |> required "albumName" string
        |> required "albumArtistId" int
        |> required "albumTracks" (list decodeTrack)

encodeAlbum : Album -> Json.Encode.Value
encodeAlbum x =
    Json.Encode.object
        [ ( "albumId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.albumId )
        , ( "albumName", Json.Encode.string x.albumName )
        , ( "albumArtistId", Json.Encode.int x.albumArtistId )
        , ( "albumTracks", (Json.Encode.list << List.map encodeTrack) x.albumTracks )
        ]

type alias Track =
    { trackId : Maybe (Int)
    , trackName : String
    , trackDuration : Int
    }

decodeTrack : Decoder Track
decodeTrack =
    decode Track
        |> required "trackId" (maybe int)
        |> required "trackName" string
        |> required "trackDuration" int

encodeTrack : Track -> Json.Encode.Value
encodeTrack x =
    Json.Encode.object
        [ ( "trackId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.trackId )
        , ( "trackName", Json.Encode.string x.trackName )
        , ( "trackDuration", Json.Encode.int x.trackDuration )
        ]