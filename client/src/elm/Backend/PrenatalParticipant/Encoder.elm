module Backend.PrenatalParticipant.Encoder exposing (encodeDeliveryLocation, encodePregnancyOutcome, encodePrenatalParticipant, pregnancyOutcomeToString)

import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodePrenatalParticipant : PrenatalParticipant -> Value
encodePrenatalParticipant data =
    object
        [ ( "person", encodeEntityUuid data.person )
        , ( "encounter_type", encodeEncounterType data.encounterType )
        , ( "expected"
          , object
                [ ( "value", encodeYYYYMMDD data.startDate )
                , ( "value2", maybe encodeYYYYMMDD data.endDate )
                ]
          )
        ]


encodeEncounterType : EncounterType -> Value
encodeEncounterType type_ =
    case type_ of
        AntenatalEncounter ->
            string "antenatal"

        InmmunizationEncounter ->
            string "inmmunization"

        NutritionEncounter ->
            string "nutrition"


encodePregnancyOutcome : PregnancyOutcome -> Value
encodePregnancyOutcome outcome =
    pregnancyOutcomeToString outcome |> string


encodeDeliveryLocation : Bool -> Value
encodeDeliveryLocation isFacilityDelivery =
    let
        location =
            if isFacilityDelivery then
                "facility"

            else
                "home"
    in
    string location


pregnancyOutcomeToString : PregnancyOutcome -> String
pregnancyOutcomeToString outcome =
    case outcome of
        OutcomeLiveAtTerm ->
            "live-at-term"

        OutcomeLivePreTerm ->
            "live-pre-term"

        OutcomeStillAtTerm ->
            "still-at-term"

        OutcomeStillPreTerm ->
            "still-pre-term"

        OutcomeAbortions ->
            "abortions"
