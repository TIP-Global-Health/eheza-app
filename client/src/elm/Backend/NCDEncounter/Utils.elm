module Backend.NCDEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import Translate exposing (Language)


getNCDEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( NCDEncounterId, NCDEncounter )
getNCDEncountersForParticipant db participantId =
    Dict.get participantId db.ncdEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []


progressReportInitiatorFromUrlFragmemt : String -> Maybe NCDProgressReportInitiator
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


progressReportInitiatorToUrlFragmemt : NCDProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorRecurrentEncounterPage encounterId ->
            "recurrent-encounter-page-" ++ fromEntityUuid encounterId
