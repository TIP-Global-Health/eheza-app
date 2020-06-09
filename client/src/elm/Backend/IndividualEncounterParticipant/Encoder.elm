module Backend.IndividualEncounterParticipant.Encoder exposing
    ( encodeDeliveryLocation
    , encodeIndividualEncounterParticipant
    , encodePregnancyOutcome
    , pregnancyOutcomeToString
    )

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (encoudeIndividualEncounterTypeAsString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeIndividualEncounterParticipant : IndividualEncounterParticipant -> Value
encodeIndividualEncounterParticipant data =
    object
        [ ( "person", encodeEntityUuid data.person )
        , ( "encounter_type", encodeIndividualEncounterType data.encounterType )
        , ( "expected"
          , object
                [ ( "value", encodeYYYYMMDD data.startDate )
                , ( "value2", maybe encodeYYYYMMDD data.endDate )
                ]
          )
        , ( "shard", maybe encodeEntityUuid data.shard )
        , ( "type", string "individual_participant" )
        ]


encodeIndividualEncounterType : IndividualEncounterType -> Value
encodeIndividualEncounterType type_ =
    encoudeIndividualEncounterTypeAsString type_ |> string


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
