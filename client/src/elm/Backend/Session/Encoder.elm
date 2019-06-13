module Backend.Session.Encoder exposing (encodeClosed, encodeSession)

import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (encodeDrupalRange, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid, fromEntityUuid)


{-| Encodes a `Session`.
-}
encodeSession : Session -> List ( String, Value )
encodeSession session =
    [ ( "scheduled_date", encodeDrupalRange encodeYYYYMMDD session.scheduledDate )
    , ( "clinic", encodeEntityUuid session.clinicId )
    , encodeClosed session.closed
    , ( "training", bool session.training )
    ]


encodeClosed : Bool -> ( String, Value )
encodeClosed closed =
    ( "closed", bool closed )
