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


recordPreganancyInitiatorFromUrlFragmemt : String -> Maybe RecordPreganancyInitiator
recordPreganancyInitiatorFromUrlFragmemt s =
    case s of
        "participant-page" ->
            Just InitiatorParticipantPage

        "warning-popup" ->
            Just InitiatorWarningPopup

        _ ->
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
            let
                split =
                    String.split "-" s
            in
            case List.head split of
                Just "encounter" ->
                    if List.length split > 1 then
                        -- Remove the "session-" prefix.
                        String.dropLeft 10 s
                            |> toEntityUuid
                            |> InitiatorNewEncounter
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
