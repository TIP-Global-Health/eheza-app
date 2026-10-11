port module SyncIncidentWorker exposing (main)

{-| Turns backend sync items into the records a device stores in `shards`.

It runs the app's own download decoder and `getDataToSendAuthority`, so the
records are exactly what a device holds, and later sends in incident details.

-}

import Json.Decode as D
import Json.Encode as E
import SyncManager.Decoder
import SyncManager.Utils


port output : E.Value -> Cmd msg


main : Program D.Value () ()
main =
    Platform.worker
        { init = \flags -> ( (), output (convert flags) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


convert : D.Value -> E.Value
convert flags =
    case D.decodeValue (D.list D.value) flags of
        Ok items ->
            E.list convertItem items

        Err err ->
            E.object [ ( "fatal", E.string (D.errorToString err) ) ]


{-| Each item is decoded as a download batch of its own, so a failure names
that item instead of failing them all.
-}
convertItem : D.Value -> E.Value
convertItem item =
    let
        uuid =
            D.decodeValue (D.field "uuid" D.string) item
                |> Result.withDefault ""

        batch =
            E.object
                [ ( "data"
                  , E.object
                        [ ( "batch", E.list identity [ item ] )
                        , ( "revision_count", E.int 0 )
                        ]
                  )
                ]
    in
    case D.decodeValue SyncManager.Decoder.decodeDownloadSyncResponseAuthority batch of
        Ok response ->
            case response.entities of
                [ entity ] ->
                    E.object
                        [ ( "uuid", E.string uuid )
                        , ( "row", E.string (String.concat (SyncManager.Utils.getDataToSendAuthority entity [])) )
                        ]

                other ->
                    E.object
                        [ ( "uuid", E.string uuid )
                        , ( "decodeError", E.string ("Decoded to " ++ String.fromInt (List.length other) ++ " entities") )
                        ]

        Err err ->
            E.object
                [ ( "uuid", E.string uuid )
                , ( "decodeError", E.string (D.errorToString err) )
                ]
