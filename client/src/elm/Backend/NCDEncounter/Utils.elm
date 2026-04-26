module Backend.NCDEncounter.Utils exposing (progressReportInitiatorFromUrlFragment, progressReportInitiatorToUrlFragment)

import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


progressReportInitiatorFromUrlFragment : String -> Maybe NCDProgressReportInitiator
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

    else
        Nothing


progressReportInitiatorToUrlFragment : NCDProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorRecurrentEncounterPage encounterId ->
            "recurrent-encounter-page-" ++ fromEntityUuid encounterId
