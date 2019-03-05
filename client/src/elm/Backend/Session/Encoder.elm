module Backend.Session.Encoder exposing (encodeClosed, encodeOfflineSession, encodeOfflineSessionWithId, encodeSession, encodeTrainingSessionAction, encodeTrainingSessionRequest)

import Backend.Child.Encoder exposing (encodeChild)
import Backend.Entities exposing (..)
import Backend.Model exposing (TrainingSessionAction(..), TrainingSessionRequest)
import Backend.Mother.Encoder exposing (encodeMother)
import Backend.Session.Model exposing (..)
import EveryDictList
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


encodeOfflineSession : OfflineSession -> List ( String, Value )
encodeOfflineSession offline =
    -- The first three encode the data for this particular session
    [ ( "scheduled_date", encodeDrupalRange encodeYYYYMMDD offline.session.scheduledDate )
    , ( "clinic", encodeEntityUuid offline.session.clinicId )
    , ( "closed", bool offline.session.closed )
    , ( "participants"
      , object
            [ ( "mothers"
              , EveryDictList.toList offline.mothers
                    |> List.map (\( id, mother ) -> object (( "id", encodeEntityUuid id ) :: encodeMother mother))
                    |> list
              )
            , ( "children"
              , EveryDictList.toList offline.children
                    |> List.map (\( id, child ) -> object (( "id", encodeEntityUuid id ) :: encodeChild child))
                    |> list
              )
            ]
      )
    ]


{-| We use this for caching to local storage.
-}
encodeOfflineSessionWithId : SessionId -> OfflineSession -> Value
encodeOfflineSessionWithId sessionId session =
    object <|
        ( "id", encodeEntityUuid sessionId )
            :: encodeOfflineSession session
