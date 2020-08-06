module Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant, pregnancyOutcomeFromString)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeIndividualEncounterParticipant : Decoder IndividualEncounterParticipant
decodeIndividualEncounterParticipant =
    succeed IndividualEncounterParticipant
        |> required "person" decodeEntityUuid
        |> required "encounter_type" decodeIndividualEncounterType
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "expected_date_concluded" (nullable decodeYYYYMMDD) Nothing
        |> optional "date_concluded" (nullable decodeYYYYMMDD) Nothing
        |> optional "outcome" (nullable decodePregnancyOutcome) Nothing
        |> optional "outcome_location" (nullable decodeDeliveryLocation) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeIndividualEncounterType : Decoder IndividualEncounterType
decodeIndividualEncounterType =
    string
        |> andThen
            (\s ->
                decodeIndividualEncounterTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized EncounterType" |> fail)
            )


decodePregnancyOutcome : Decoder PregnancyOutcome
decodePregnancyOutcome =
    string
        |> andThen
            (\s ->
                pregnancyOutcomeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized PregnancyOutcome" |> fail)
            )


pregnancyOutcomeFromString : String -> Maybe PregnancyOutcome
pregnancyOutcomeFromString outcome =
    case outcome of
        "live-at-term" ->
            Just OutcomeLiveAtTerm

        "live-pre-term" ->
            Just OutcomeLivePreTerm

        "still-at-term" ->
            Just OutcomeStillAtTerm

        "still-pre-term" ->
            Just OutcomeStillPreTerm

        "abortions" ->
            Just OutcomeAbortions

        _ ->
            Nothing


decodeDeliveryLocation : Decoder DeliveryLocation
decodeDeliveryLocation =
    string
        |> andThen
            (\s ->
                case s of
                    "facility" ->
                        succeed FacilityDelivery

                    "home" ->
                        succeed HomeDelivery

                    _ ->
                        s ++ " is not a recognized DeliveryLocation" |> fail
            )
