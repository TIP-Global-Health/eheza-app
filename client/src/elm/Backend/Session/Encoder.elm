module Backend.Session.Encoder exposing (encodeSession)

import Backend.Clinic.Encoder exposing (encodeClinicType)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (encodeDrupalRange, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid, fromEntityUuid)


{-| Encodes a `Session`.
-}
encodeSession : Session -> List ( String, Value )
encodeSession session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "clinic", encodeEntityUuid session.clinicId )
    , ( "clinic_type", encodeClinicType session.clinicType )
    ]
