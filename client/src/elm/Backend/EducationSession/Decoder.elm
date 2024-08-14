module Backend.EducationSession.Decoder exposing (decodeEducationSession, decodeEducationTopic)

import Backend.EducationSession.Model exposing (..)
import Backend.EducationSession.Utils exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeEverySet)


decodeEducationSession : Decoder EducationSession
decodeEducationSession =
    succeed EducationSession
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> required "nurse" decodeEntityUuid
        |> required "village_ref" decodeEntityUuid
        |> optional "education_topics" (decodeEverySet decodeEducationTopic) EverySet.empty
        |> optional "participating_patients" (decodeEverySet decodeEntityUuid) EverySet.empty
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeEducationTopic : Decoder EducationTopic
decodeEducationTopic =
    string
        |> andThen
            (\sign ->
                educationTopicFromString sign
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| sign ++ " is not a recognized EducationTopic")
            )
