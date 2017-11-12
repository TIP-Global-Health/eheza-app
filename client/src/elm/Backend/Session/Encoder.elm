module Backend.Session.Encoder exposing (..)

import Backend.Child.Encoder exposing (encodeChild)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Mother.Encoder exposing (encodeMother)
import Backend.Session.Model exposing (..)
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (encodeDrupalRange, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityId)


{-| Encodes a `Session`.
-}
encodeSession : Session -> Value
encodeSession session =
    object
        [ ( "scheduled_date", encodeDrupalRange encodeYYYYMMDD session.scheduledDate )
        , ( "clinic", encodeEntityId session.clinicId )
        ]


encodeOfflineSession : OfflineSession -> Value
encodeOfflineSession offline =
    object
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

                -- TODO: Actually encode this
                , ( "mother_activity", list [] )
                , ( "child_activity", list [] )
                ]
          )
        ]
