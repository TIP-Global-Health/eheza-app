module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model exposing (ClinicalProgressReportInitiator(..), RecordPreganancyInitiator(..))
import List.Extra
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


recordPreganancyInitiatorToUrlFragmemt : RecordPreganancyInitiator -> String
recordPreganancyInitiatorToUrlFragmemt initiator =
    case initiator of
        InitiatorParticipantPage ->
            "participant-page"

        InitiatorWarningPopup ->
            "warning-popup"

        InitiatorPostpartumEncounter ->
            "postpartum-encounter"


recordPreganancyInitiatorFromUrlFragmemt : String -> Maybe RecordPreganancyInitiator
recordPreganancyInitiatorFromUrlFragmemt s =
    case s of
        "participant-page" ->
            Just InitiatorParticipantPage

        "warning-popup" ->
            Just InitiatorWarningPopup

        "postpartum-encounter" ->
            Just InitiatorPostpartumEncounter

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
                    -- Second element is the UUID of the session.
                    List.Extra.getAt 1 split
                        |> Maybe.map (toEntityUuid >> InitiatorNewEncounter)

                _ ->
                    Nothing
