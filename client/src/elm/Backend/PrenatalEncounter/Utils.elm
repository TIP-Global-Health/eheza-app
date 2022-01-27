module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..), RecordPreganancyInitiator(..))
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


recordPreganancyInitiatorToUrlFragmemt : RecordPreganancyInitiator -> String
recordPreganancyInitiatorToUrlFragmemt initiator =
    case initiator of
        InitiatorParticipantPage ->
            "participant-page"

        InitiatorWarningPopup ->
            "warning-popup"

        InitiatorPostpartumEncounter encounterId ->
            "postpartum-encounter-" ++ fromEntityUuid encounterId


recordPreganancyInitiatorFromUrlFragmemt : String -> Maybe RecordPreganancyInitiator
recordPreganancyInitiatorFromUrlFragmemt s =
    case s of
        "participant-page" ->
            Just InitiatorParticipantPage

        "warning-popup" ->
            Just InitiatorWarningPopup

        _ ->
            if String.startsWith "postpartum-encounter" s then
                String.dropLeft (String.length "postpartum-encounter-") s
                    |> toEntityUuid
                    |> InitiatorPostpartumEncounter
                    |> Just

            else
                Nothing


progressReportInitiatorToUrlFragmemt : PrenatalProgressReportInitiator -> String
progressReportInitiatorToUrlFragmemt initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorNewEncounter encounterId ->
            "encounter-" ++ fromEntityUuid encounterId

        InitiatorPatientRecord personId ->
            "patient-record-" ++ fromEntityUuid personId


progressReportInitiatorFromUrlFragmemt : String -> Maybe PrenatalProgressReportInitiator
progressReportInitiatorFromUrlFragmemt s =
    if String.startsWith "encounter-page" s then
        String.dropLeft (String.length "encounter-page-") s
            |> toEntityUuid
            |> InitiatorEncounterPage
            |> Just

    else if String.startsWith "encounter" s then
        String.dropLeft (String.length "encounter-") s
            |> toEntityUuid
            |> InitiatorNewEncounter
            |> Just

    else if String.startsWith "patient-record" s then
        String.dropLeft (String.length "patient-record-") s
            |> toEntityUuid
            |> InitiatorPatientRecord
            |> Just

    else
        Nothing
