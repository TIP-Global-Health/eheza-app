port module CacheStorage.Update exposing (subscriptions, update)

{-| Interact with the `CacheStorage` APIs.
-}

import CacheStorage.Model exposing (..)
import Json.Decode exposing (Decoder, decodeValue, fail, field)
import Json.Encode exposing (Value, object)
import RemoteData exposing (RemoteData(..))


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update accessToken msg model =
    case msg of
        SendRequest request ->
            ( { model | cachedPhotos = Loading }
            , cacheStorageRequest (encodeRequest accessToken request)
            )

        HandleResponse value ->
            case decodeValue decodeResponse value of
                Ok response ->
                    case response of
                        SetCachedPhotos urls ->
                            ( { model | cachedPhotos = Success urls }
                            , Cmd.none
                            )

                Err err ->
                    -- TODO: Reflect the err in the cachedPhotos
                    let
                        _ =
                            Debug.log <|
                                "Error decoding message from port: "
                                    ++ err
                    in
                    ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    cacheStorageResponse HandleResponse


{-| Receive messages about the cache.
-}
port cacheStorageResponse : (Value -> msg) -> Sub msg


{-| Sends messages about the cache.
-}
port cacheStorageRequest : Value -> Cmd msg


encodeRequest : String -> Request -> Value
encodeRequest accessToken request =
    case request of
        CachePhotos urls ->
            object
                [ ( "tag", Json.Encode.string "CachePhotos" )
                , ( "access_token", Json.Encode.string accessToken )
                , ( "value", Json.Encode.list <| List.map Json.Encode.string urls )
                ]

        CheckCachedPhotos ->
            object
                [ ( "tag", Json.Encode.string "CheckCachedPhotos" )
                ]

        ClearCachedPhotos ->
            object
                [ ( "tag", Json.Encode.string "ClearCachedPhotos" )
                ]


decodeResponse : Decoder Response
decodeResponse =
    field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "SetCachedPhotos" ->
                        field "value" (Json.Decode.list Json.Decode.string)
                            |> Json.Decode.map SetCachedPhotos

                    _ ->
                        fail <|
                            "Unrecognized tag: "
                                ++ tag
            )
