module Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)

import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


{-| Encodes a `WellChildEncounter`.
-}
encodeWellChildEncounter : WellChildEncounter -> List ( String, Value )
encodeWellChildEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "well_child_encounter_type", encodeWellChildEncounterType session.encounterType )
    , ( "deleted", bool False )
    , ( "type", string "well_child_encounter" )
    ]
        ++ encodeIfExists "shard" session.shard encodeEntityUuid


encodeWellChildEncounterType : WellChildEncounterType -> Value
encodeWellChildEncounterType encounterType =
    string <|
        case encounterType of
            NewbornExam ->
                "newborn-exam"

            PediatricCareBirthTo6Weeks ->
                "pediatric-care-birth-to-6w"

            PediatricCare6Weeks ->
                "pediatric-care-6w"

            PediatricCare10Weeks ->
                "pediatric-care-10w"

            PediatricCare14Weeks ->
                "pediatric-care-14w"

            PediatricCare6Months ->
                "pediatric-care-6m"

            PediatricCare9Months ->
                "pediatric-care-9m"

            PediatricCare12Months ->
                "pediatric-care-12m"

            PediatricCare15Months ->
                "pediatric-care-15m"

            PediatricCare18Months ->
                "pediatric-care-18m"

            PediatricCareRecurrent ->
                "pediatric-care"
