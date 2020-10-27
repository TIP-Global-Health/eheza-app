module Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessEncounter)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.AcuteIllnessEncounter.Utils exposing (acuteIllnessDiagnosisToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


{-| Encodes a `AcuteIllnessEncounter`.
-}
encodeAcuteIllnessEncounter : AcuteIllnessEncounter -> List ( String, Value )
encodeAcuteIllnessEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "acute_illness_diagnosis", encodeAcuteIllnessDiagnosis session.diagnosis )
    , ( "shard", maybe encodeEntityUuid session.shard )
    ]


encodeAcuteIllnessDiagnosis : AcuteIllnessDiagnosis -> Value
encodeAcuteIllnessDiagnosis diagnosis =
    acuteIllnessDiagnosisToString diagnosis |> string
