module Backend.IndividualEncounterParticipant.Decoder exposing (..)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (individualEncounterTypeFromString)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
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
        |> optional "outcome" (nullable decodeIndividualEncounterParticipantOutcome) Nothing
        |> optional "outcome_location" (nullable decodeDeliveryLocation) Nothing
        |> optional "newborn" (nullable decodeEntityUuid) Nothing
        |> required "deleted" bool
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeIndividualEncounterType : Decoder IndividualEncounterType
decodeIndividualEncounterType =
    string
        |> andThen
            (\s ->
                individualEncounterTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized EncounterType" |> fail)
            )


decodeIndividualEncounterParticipantOutcome : Decoder IndividualEncounterParticipantOutcome
decodeIndividualEncounterParticipantOutcome =
    string
        |> andThen
            (\s ->
                case pregnancyOutcomeFromString s of
                    Just pregnancyOutcome ->
                        succeed (Pregnancy pregnancyOutcome)

                    Nothing ->
                        acuteIllnessOutcomeFromString s
                            |> Maybe.map (AcuteIllness >> succeed)
                            |> Maybe.withDefault (s ++ " is not a recognized IndividualEncounterParticipantOutcome" |> fail)
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


acuteIllnessOutcomeFromString : String -> Maybe AcuteIllnessOutcome
acuteIllnessOutcomeFromString outcome =
    case outcome of
        "illness-resolved" ->
            Just OutcomeIllnessResolved

        "lost-to-follow-up" ->
            Just OutcomeLostToFollowUp

        "moved-out-of-ca" ->
            Just OutcomeMovedOutsideCA

        "patient-died" ->
            Just OutcomePatientDied

        "referred-to-hc" ->
            Just OutcomeReferredToHC

        "other" ->
            Just OutcomeOther

        _ ->
            Nothing
