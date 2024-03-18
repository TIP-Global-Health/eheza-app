module Backend.EducationSession.Encoder exposing (encodeEducationSession)

import Backend.EducationSession.Model exposing (..)
import Backend.EducationSession.Utils exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeEverySet, encodeIfSet)


{-| Encodes a `EducationSession`.
-}
encodeEducationSession : EducationSession -> List ( String, Value )
encodeEducationSession session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "nurse", encodeEntityUuid session.nurse )
    , ( "village_ref", encodeEntityUuid session.village )
    , ( "education_topics", encodeEverySet encodeEducationTopic session.topics )
    , ( "participating_patients", encodeEverySet encodeEntityUuid session.participants )
    , ( "deleted", bool False )
    , ( "type", string "education_session" )
    ]
        ++ encodeIfSet "shard" session.shard encodeEntityUuid


encodeEducationTopic : EducationTopic -> Value
encodeEducationTopic =
    educationTopicToString >> string
