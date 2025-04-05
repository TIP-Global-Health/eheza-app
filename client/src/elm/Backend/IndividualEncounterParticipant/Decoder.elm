module Backend.IndividualEncounterParticipant.Decoder exposing (..)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeIndividualEncounterParticipant : Decoder IndividualEncounterParticipant
decodeIndividualEncounterParticipant =
    succeed IndividualEncounterParticipant
        |> required "person" decodeEntityUuid
        |> required "encounter_type" decodeIndividualEncounterType
        |> required "start_date" decodeYYYYMMDD
        |> optional "end_date" (nullable decodeYYYYMMDD) Nothing
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
                        case tuberculosisOutcomeFromString s of
                            Just tuberculosisOutcome ->
                                succeed (Tuberculosis tuberculosisOutcome)

                            Nothing ->
                                case hivOutcomeFromString s of
                                    Just hivOutcome ->
                                        succeed (HIV hivOutcome)

                                    Nothing ->
                                        acuteIllnessOutcomeFromString s
                                            |> Maybe.map (AcuteIllness >> succeed)
                                            |> Maybe.withDefault (s ++ " is not a recognized IndividualEncounterParticipantOutcome" |> fail)
            )


decodeDeliveryLocation : Decoder DeliveryLocation
decodeDeliveryLocation =
    string
        |> andThen
            (\s ->
                deliveryLocationFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized DeliveryLocation" |> fail)
            )
