module Backend.PrenatalParticipant.Decoder exposing (decodePrenatalParticipant, pregnancyOutcomeFromString)

import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodePrenatalParticipant : Decoder PrenatalParticipant
decodePrenatalParticipant =
    succeed PrenatalParticipant
        |> required "person" decodeEntityUuid
        |> required "encounter_type" decodeEncounterType
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "expected_date_concluded" (nullable decodeYYYYMMDD) Nothing


decodeEncounterType : Decoder EncounterType
decodeEncounterType =
    string
        |> andThen
            (\s ->
                case s of
                    "antenatal" ->
                        succeed AntenatalEncounter

                    "inmmunization" ->
                        succeed InmmunizationEncounter

                    "nutrition" ->
                        succeed NutritionEncounter

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized EncounterType"
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
