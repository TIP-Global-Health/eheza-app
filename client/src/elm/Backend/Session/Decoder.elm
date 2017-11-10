module Backend.Session.Decoder exposing (..)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Child.Model exposing (Child)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeHistoricalMeasurements)
import Backend.Measurement.Model exposing (emptyMeasurements)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed, at, oneOf)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityId)


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
        |> custom decodeHistoricalMeasurements
        -- We start with empty stuff for the `previousMeasurements`
        -- and `currentMeasurements` ... then we map to fill them in.
        |> hardcoded emptyMeasurements
        |> hardcoded emptyMeasurements
        |> map splitHistoricalMeasurements


{-| Takes the historical measurements and populates `previousMeasurements`
and `currentMeasurements` as appropriate. Anything which goes in `currentMeasurements`
is removed from `historicalMeasurments`.
-}
splitHistoricalMeasurements : OfflineSession -> OfflineSession
splitHistoricalMeasurements session =
    -- TODO: implement
    session


decodeMothers : Decoder (EveryDictList MotherId Mother)
decodeMothers =
    EveryDictList.decodeArray2 (field "id" decodeEntityId) decodeMother


decodeChildren : Decoder (EveryDict ChildId Child)
decodeChildren =
    map2 (,) (field "id" decodeEntityId) decodeChild
        |> list
        |> map EveryDict.fromList
