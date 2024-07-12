module Pages.Prenatal.Outcome.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Utils exposing (getPrenatalEncountersForParticipant)
import Maybe.Extra
import RemoteData


fetch : IndividualEncounterParticipantId -> ModelIndexedDb -> List MsgIndexedDb
fetch participantId db =
    let
        personId =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            getPrenatalEncountersForParticipant db participantId
                |> List.map Tuple.first

        lastEncounterId =
            List.reverse encountersIds
                |> List.head

        -- We fetch measurements for all encounters, to be
        -- able to resolve EGA, EDD, Gravida and Para.
        fetchMeasurements =
            List.map FetchPrenatalMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchPerson personId
        , Maybe.map FetchPrenatalEncounter lastEncounterId
        ]
        ++ [ FetchIndividualEncounterParticipant participantId, FetchPrenatalEncountersForParticipant participantId ]
        ++ fetchMeasurements
