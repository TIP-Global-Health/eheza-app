module Backend.TuberculosisEncounter.Encoder exposing (encodeTuberculosisEncounter)

import Backend.TuberculosisEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `TuberculosisEncounter`.
-}
encodeTuberculosisEncounter : TuberculosisEncounter -> List ( String, Value )
encodeTuberculosisEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "deleted", bool False )
    , ( "type", string "tuberculosis_encounter" )
    ]
        ++ encodeIfSet "shard" session.shard encodeEntityUuid
