module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model exposing (RecordPreganancyInitiator(..))


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
