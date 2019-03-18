module Backend.Session.Encoder exposing (encodeClosed, encodeSession, encodeTrainingSessionAction, encodeTrainingSessionRequest)

import Backend.Model exposing (TrainingSessionAction(..), TrainingSessionRequest)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (encodeDrupalRange, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid, fromEntityUuid)


encodeTrainingSessionRequest : TrainingSessionRequest -> Value
encodeTrainingSessionRequest req =
    object
        [ ( "action", encodeTrainingSessionAction req.action )
        ]


{-| Encodes a `TrainingSessionAction`.
-}
encodeTrainingSessionAction : TrainingSessionAction -> Value
encodeTrainingSessionAction action =
    case action of
        CreateAll ->
            string "create_all"

        DeleteAll ->
            string "delete_all"


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
