module Backend.FamilyEncounterParticipant.Utils exposing (familyEncounterTypeFromString, familyEncounterTypeToString, initiatorFromUrlFragment, initiatorToUrlFragment)

import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterType(..), FamilyParticipantInitiator(..))
import Backend.PatientRecord.Utils
import Maybe.Extra
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
