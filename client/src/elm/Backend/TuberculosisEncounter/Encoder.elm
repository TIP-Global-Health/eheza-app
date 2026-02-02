module Backend.TuberculosisEncounter.Encoder exposing (encodeTuberculosisEncounter)

import Backend.TuberculosisEncounter.Model exposing (TuberculosisEncounter)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `TuberculosisEncounter`.
-}
encodeTuberculosisEncounter : TuberculosisEncounter -> List ( String, Value )
encodeTuberculosisEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "tuberculosis_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
