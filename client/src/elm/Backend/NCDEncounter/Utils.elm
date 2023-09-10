module Backend.NCDEncounter.Utils exposing (getNCDEncountersForParticipant, progressReportInitiatorFromUrlFragment, progressReportInitiatorToUrlFragment)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


getNCDEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( NCDEncounterId, NCDEncounter )
getNCDEncountersForParticipant db participantId =
    Dict.get participantId db.ncdEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []


progressReportInitiatorFromUrlFragment : String -> Maybe NCDProgressReportInitiator
progressReportInitiatorFromUrlFragment s =
    if String.startsWith "encounter-page-" s then
        String.dropLeft (String.length "encounter-page-") s
            |> toEntityUuid
            |> InitiatorEncounterPage
            |> Just

    else if String.startsWith "recurrent-encounter-page-" s then
        String.dropLeft (String.length "recurrent-encounter-page-") s
            |> toEntityUuid
            |> InitiatorRecurrentEncounterPage
            |> Just

    else
        Nothing


progressReportInitiatorToUrlFragment : NCDProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorRecurrentEncounterPage encounterId ->
            "recurrent-encounter-page-" ++ fromEntityUuid encounterId
