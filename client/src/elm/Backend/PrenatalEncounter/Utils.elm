module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model exposing (ClinicalProgressReportInitiator(..), RecordPreganancyInitiator(..))
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


progressReportInitiatorToUrlFragmemt : ClinicalProgressReportInitiator -> String
progressReportInitiatorToUrlFragmemt initiator =
    case initiator of
        InitiatorEncounterPage ->
            "encounter-page"

        InitiatorNewEncounter encounterId ->
            "encounter-" ++ fromEntityUuid encounterId


progressReportInitiatorFromUrlFragmemt : String -> Maybe ClinicalProgressReportInitiator
progressReportInitiatorFromUrlFragmemt s =
    case s of
        "encounter-page" ->
            Just InitiatorEncounterPage

        _ ->
            if String.startsWith "encounter" s then
                String.dropLeft (String.length "encounter-") s
                    |> toEntityUuid
                    |> InitiatorNewEncounter
                    |> Just

            else
                Nothing
