module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounterType(..)
        , PrenatalProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


recordPreganancyInitiatorToUrlFragment : RecordPreganancyInitiator -> String
recordPreganancyInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantPage ->
            "participant-page"

        InitiatorWarningPopup ->
            "warning-popup"

        InitiatorPostpartumEncounter encounterId ->
            "postpartum-encounter-" ++ fromEntityUuid encounterId


recordPreganancyInitiatorFromUrlFragment : String -> Maybe RecordPreganancyInitiator
recordPreganancyInitiatorFromUrlFragment s =
    case s of
        "participant-page" ->
            Just InitiatorParticipantPage

        "warning-popup" ->
            Just InitiatorWarningPopup

        _ ->
            if String.startsWith "postpartum-encounter-" s then
                String.dropLeft 21 s
                    |> toEntityUuid
                    |> InitiatorPostpartumEncounter
                    |> Just

            else
                Nothing


progressReportInitiatorToUrlFragment : PrenatalProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorRecurrentEncounterPage encounterId ->
            "recurrent-encounter-page-" ++ fromEntityUuid encounterId

        InitiatorNewEncounter encounterId ->
            "encounter-" ++ fromEntityUuid encounterId

        InitiatorPatientRecord personId ->
            "patient-record-" ++ fromEntityUuid personId

        InitiatorCaseManagement encounterId ->
            "case-management-" ++ fromEntityUuid encounterId


progressReportInitiatorFromUrlFragment : String -> Maybe PrenatalProgressReportInitiator
progressReportInitiatorFromUrlFragment s =
    if String.startsWith "encounter-page-" s then
        String.dropLeft 15 s
            |> toEntityUuid
            |> InitiatorEncounterPage
            |> Just

    else if String.startsWith "recurrent-encounter-page-" s then
        String.dropLeft 25 s
            |> toEntityUuid
            |> InitiatorRecurrentEncounterPage
            |> Just

    else if String.startsWith "encounter-" s then
        String.dropLeft 10 s
            |> toEntityUuid
            |> InitiatorNewEncounter
            |> Just

    else if String.startsWith "patient-record-" s then
        String.dropLeft 15 s
            |> toEntityUuid
            |> InitiatorPatientRecord
            |> Just

    else if String.startsWith "case-management-" s then
        String.dropLeft 16 s
            |> toEntityUuid
            |> InitiatorCaseManagement
            |> Just

    else
        Nothing


{-| LMP date is considered to be the day on which pregnancy has started.
-EDD date is estimated delivery date - the day on which we expect pregnancy
-be concluded.
-Pregnancy lasts 280 days.
-}
lmpToEDDDate : NominalDate -> NominalDate
lmpToEDDDate lmpDate =
    Date.add Days 280 lmpDate


eddToLmpDate : NominalDate -> NominalDate
eddToLmpDate eddDate =
    Date.add Days -280 eddDate


isNurseEncounter : PrenatalEncounterType -> Bool
isNurseEncounter encounterType =
    List.member encounterType [ NurseEncounter, NursePostpartumEncounter ]
