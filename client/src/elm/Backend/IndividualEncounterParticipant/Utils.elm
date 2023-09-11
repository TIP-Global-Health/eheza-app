module Backend.IndividualEncounterParticipant.Utils exposing (individualEncounterTypeFromString, individualEncounterTypeToString, initiatorFromUrlFragment, initiatorToUrlFragment, isDailyEncounterActive)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.PatientRecord.Utils
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


individualEncounterTypeToString : IndividualEncounterType -> String
individualEncounterTypeToString encounterType =
    case encounterType of
        AcuteIllnessEncounter ->
            "acute-illness"

        AntenatalEncounter ->
            "antenatal"

        HomeVisitEncounter ->
            "home-visit"

        InmmunizationEncounter ->
            "inmmunization"

        NutritionEncounter ->
            "nutrition"

        WellChildEncounter ->
            "well-child"

        NCDEncounter ->
            "ncd"


individualEncounterTypeFromString : String -> Maybe IndividualEncounterType
individualEncounterTypeFromString string =
    case string of
        "acute-illness" ->
            Just AcuteIllnessEncounter

        "antenatal" ->
            Just AntenatalEncounter

        "home-visit" ->
            Just HomeVisitEncounter

        "inmmunization" ->
            Just InmmunizationEncounter

        "nutrition" ->
            Just NutritionEncounter

        "well-child" ->
            Just WellChildEncounter

        "ncd" ->
            Just NCDEncounter

        _ ->
            Nothing


isDailyEncounterActive : NominalDate -> { a | startDate : NominalDate, endDate : Maybe NominalDate } -> Bool
isDailyEncounterActive currentDate encounter =
    encounter.startDate == currentDate && isNothing encounter.endDate


initiatorToUrlFragment : IndividualParticipantInitiator -> String
initiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantsPage ->
            "participants-page"

        InitiatorPatientRecord patientRecordInitiator personId ->
            "patient-record-"
                ++ fromEntityUuid personId
                ++ "+++"
                ++ Backend.PatientRecord.Utils.progressReportInitiatorToUrlFragment patientRecordInitiator


initiatorFromUrlFragment : String -> Maybe IndividualParticipantInitiator
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
