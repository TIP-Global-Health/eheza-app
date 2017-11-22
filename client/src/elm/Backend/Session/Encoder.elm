module Backend.Session.Encoder exposing (..)

import Backend.Child.Encoder exposing (encodeChild)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (encodeChildMeasurementList, encodeMotherMeasurementList)
import Backend.Mother.Encoder exposing (encodeMother)
import Backend.Session.Model exposing (..)
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (encodeDrupalRange, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityId, fromEntityId)


{-| Encodes a `Session`.
-}
encodeSession : Session -> Value
encodeSession session =
    object
        [ ( "scheduled_date", encodeDrupalRange encodeYYYYMMDD session.scheduledDate )
        , ( "clinic", encodeEntityId session.clinicId )
        , ( "closed", bool session.closed )
        ]


encodeOfflineSession : OfflineSession -> List ( String, Value )
encodeOfflineSession offline =
    [ ( "scheduled_date", encodeDrupalRange encodeYYYYMMDD offline.session.scheduledDate )
    , ( "clinic_id", encodeEntityId offline.session.clinicId )
    , ( "clinic", encodeClinic offline.clinic )
    , ( "participants"
      , object
            [ ( "mothers"
              , EveryDictList.toList offline.mothers
                    |> List.map (\( id, mother ) -> object (( "id", encodeEntityId id ) :: encodeMother mother))
                    |> list
              )
            , ( "children"
              , EveryDict.toList offline.children
                    |> List.map (\( id, child ) -> object (( "id", encodeEntityId id ) :: encodeChild child))
                    |> list
              )
            , ( "mother_activity"
              , EveryDict.toList offline.historicalMeasurements.mothers
                    |> List.map (Tuple.mapFirst (fromEntityId >> toString) >> Tuple.mapSecond encodeMotherMeasurementList)
                    |> object
              )
            , ( "child_activity"
              , EveryDict.toList offline.historicalMeasurements.children
                    |> List.map (Tuple.mapFirst (fromEntityId >> toString) >> Tuple.mapSecond encodeChildMeasurementList)
                    |> object
              )
            ]
      )
    ]


{-| We use this for caching to local storage.
-}
encodeOfflineSessionWithId : SessionId -> OfflineSession -> Value
encodeOfflineSessionWithId sessionId session =
    object <|
        ( "id", encodeEntityId sessionId )
            :: encodeOfflineSession session
