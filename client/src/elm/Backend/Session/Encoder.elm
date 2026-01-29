module Backend.Session.Encoder exposing (encodeSession)

import Backend.Clinic.Encoder exposing (encodeClinicType)
import Backend.Session.Model exposing (Session)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


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
    , ( "deleted", bool session.deleted )
    , ( "type", string "session" )
    ]
