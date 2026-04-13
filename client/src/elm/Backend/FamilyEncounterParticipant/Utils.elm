module Backend.FamilyEncounterParticipant.Utils exposing (..)

import Backend.FamilyEncounterParticipant.Model exposing (..)
import Backend.PatientRecord.Utils
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


familyEncounterTypeToString : FamilyEncounterType -> String
familyEncounterTypeToString encounterType =
    case encounterType of
        NutritionEncounter ->
            "nutrition"


familyEncounterTypeFromString : String -> Maybe FamilyEncounterType
familyEncounterTypeFromString string =
    case string of
        "nutrition" ->
            Just NutritionEncounter

        _ ->
            Nothing


isDailyEncounterActive : NominalDate -> { a | startDate : NominalDate, endDate : Maybe NominalDate } -> Bool
isDailyEncounterActive currentDate encounter =
    encounter.startDate == currentDate && isNothing encounter.endDate


initiatorToUrlFragment : FamilyParticipantInitiator -> String
initiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantsPage ->
            "participants-page"

        InitiatorPatientRecord patientRecordInitiator personId ->
            "patient-record-"
                ++ fromEntityUuid personId
                ++ "+++"
                ++ Backend.PatientRecord.Utils.progressReportInitiatorToUrlFragment patientRecordInitiator


initiatorFromUrlFragment : String -> Maybe FamilyParticipantInitiator
initiatorFromUrlFragment s =
    case s of
        "participants-page" ->
            Just InitiatorParticipantsPage

        _ ->
            if String.startsWith "patient-record-" s then
                let
                    fragments =
                        String.dropLeft 15 s
                            |> String.split "+++"
                in
                if List.length fragments /= 2 then
                    Nothing

                else
                    Maybe.map2
                        (\personId patientRecordInitiator ->
                            Just <| InitiatorPatientRecord patientRecordInitiator (toEntityUuid personId)
                        )
                        (List.head fragments)
                        (List.drop 1 fragments
                            |> List.head
                            |> Maybe.andThen Backend.PatientRecord.Utils.progressReportInitiatorFromUrlFragment
                        )
                        |> Maybe.Extra.join

            else
                Nothing
