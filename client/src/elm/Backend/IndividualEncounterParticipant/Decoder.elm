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
        |> hardcoded Nothing


decodeIndividualEncounterType : Decoder IndividualEncounterType
decodeIndividualEncounterType =
    string
        |> andThen
            (\s ->
                decodeIndividualEncounterTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized EncounterType" |> fail)
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


acuteIllnessOutcomeFromString : String -> Maybe AcuteIllnessOutcome
acuteIllnessOutcomeFromString outcome =
    case outcome of
        "lillness-resolved" ->
            Just OutcomeIllnessResolved

        "lost-to-follow-up" ->
            Just OutcomeLostToFollowUp

        "patient-died" ->
            Just OutcomePatientDied

        "referred-to-hc" ->
            Just OutcomeReferredToHC

        "other" ->
            Just OutcomeOther

        _ ->
            Nothing
