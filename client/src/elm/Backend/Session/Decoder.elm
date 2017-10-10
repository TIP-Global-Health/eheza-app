module Backend.Session.Decoder exposing (..)

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Session.Model exposing (..)
import Drupal.Restful exposing (decodeEntityId)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed, at, oneOf)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)


{-| Decodes the JSON sent by /api/sessions
-}
decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> custom
            (oneOf
                -- Work with "full_view" true or false
                [ field "clinic" decodeEntityId
                , at [ "clinic", "id" ] decodeEntityId
                ]
            )


{-| Decodes the JSON sent by /api/offline_sessions
-}
decodeOfflineSession : Decoder OfflineSession
decodeOfflineSession =
    decode OfflineSession
        -- For the "basic" session data, we can reuse the decoder
        |> custom decodeSession
        -- we get the full clinic here, as a convenience for going offline
        |> required "clinic" decodeClinic
        |> requiredAt [ "participants", "mothers" ] decodeMothers
        |> requiredAt [ "participants", "children" ] decodeChildren
        |> hardcoded EveryDict.empty
        |> hardcoded EveryDict.empty


decodeMothers : Decoder (EveryDictList MotherId Mother)
decodeMothers =
    EveryDictList.decodeArray2 (field "id" decodeEntityId) decodeMother


decodeChildren : Decoder (EveryDict ChildId Child)
decodeChildren =
    Debug.crash "unimplemented"
